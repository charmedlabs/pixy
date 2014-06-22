//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#include <string.h>
#include <stdio.h>
#include "pixyinterpreter.hpp"
#include "pixytypes.h"

PixyInterpreter::PixyInterpreter()
{
  thread_die_  = false;
  thread_dead_ = true;
  receiver_    = nullptr;
}

void PixyInterpreter::init()
{

  if(thread_dead_ == false) 
  {
    fprintf(stderr, "libpixy: Already initialized.");
    return;
  }

  if(link_.open() < 0) {
    fprintf(stderr, "libpixy: Error: Unable to open USB device.\n");
    return;
  }

  receiver_ = new ChirpReceiver(&link_, this);

  // Create the interpreter thread //

  thread_dead_ = false;
  thread_      = std::thread(&PixyInterpreter::interpreter_thread, this);
}

void PixyInterpreter::close()
{
  // Is the interpreter thread alive? //
  if(thread_.joinable()) 
  {
    // Thread is running, tell the interpreter thread to die. //
    thread_die_ = true;
    thread_.join();
  } 
    
  delete receiver_;
}

uint16_t PixyInterpreter::get_blocks(uint16_t max_blocks, Block * blocks)
{
  uint16_t number_of_blocks_to_copy;
  uint16_t index;
    
  // Prevent other thread from accessing 'blocks_' while we're using it. //

  blocks_access_mutex_.lock();

  number_of_blocks_to_copy = (max_blocks >= blocks_.size() ? blocks_.size() : max_blocks);

  // Copy blocks //
  
  for (index = 0; index != number_of_blocks_to_copy; ++index) {
    memcpy(&blocks[index], &blocks_[0], sizeof(Block));
    blocks_.erase(blocks_.begin());
  }

  blocks_access_mutex_.unlock();

  return number_of_blocks_to_copy;
}

void PixyInterpreter::interpreter_thread()
{
  thread_dead_ = false;

  // Read from Pixy USB connection using the Chirp //
  // protocol until we're told to stop.            //

  while(!thread_die_) {
    receiver_->service(false);
  }

  thread_dead_ = true;
}


void PixyInterpreter::interpret_data(void * chirp_data[])
{
  uint8_t  chirp_message;
  uint32_t chirp_type;

  if (chirp_data[0]) {

    chirp_message = Chirp::getType(chirp_data[0]);

    switch(chirp_message) {
      
      case CRP_TYPE_HINT:
        
        chirp_type = * static_cast<uint32_t *>(chirp_data[0]);

        switch(chirp_type) {

          case FOURCC('B', 'A', '8', '1'):
            break;
          case FOURCC('C', 'C', 'Q', '1'):
            break;
          case FOURCC('C', 'C', 'B', '1'):
            interpret_CCB1(chirp_data + 1);
            break;
          case FOURCC('C', 'C', 'B', '2'):
            break;
          case FOURCC('C', 'M', 'V', '1'):
            break;
          default:
            printf("libpixy: Chirp hint [%u] not recognized.\n", chirp_type);
            break;

        }

        break;

      case CRP_HSTRING:

        // Pixy debug message //

        printf("libpixy (DEBUG): %s\n", static_cast<char *>(chirp_data[0]));
        break;
      
      default:
       
       fprintf(stderr, "libpixy: Unknown message received from Pixy: [%u]\n", chirp_message);
       break;
    }
  } 
}

void PixyInterpreter::interpret_CCB1(void * CCB1_data[])
{
  uint32_t   number_of_blobs;
  BlobA    * blobs;
  uint32_t   index;
  Block      block;

  number_of_blobs = * static_cast<uint32_t *>(CCB1_data[3]);
  blobs           = static_cast<BlobA *>(CCB1_data[4]);
  
  number_of_blobs /= sizeof(BlobA) / sizeof(uint16_t);
  
  for (index = 0; index != number_of_blobs; ++index) {

    block.signature = blobs[index].m_model;
    block.width     = blobs[index].m_right - blobs[index].m_left;
    block.height    = blobs[index].m_bottom - blobs[index].m_top;
    block.x         = blobs[index].m_left + block.width / 2;
    block.y         = blobs[index].m_top + block.height / 2;
      
    // Store new block in block buffer //

    blocks_access_mutex_.lock();

    if (blocks_.size() == PIXY_BLOCK_CAPACITY) {
      // Blocks buffer is full - replace oldest received block with newest block //
      blocks_.erase(blocks_.begin());
      blocks_.push_back(block);
    } else {
      // Add new block to blocks buffer //
      blocks_.push_back(block);
    }
    
    blocks_access_mutex_.unlock();
  }
}
