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

PixyInterpreter::PixyInterpreter()
{
  thread_die_  = false;
  thread_dead_ = true;
  receiver_    = NULL;
}

PixyInterpreter::~PixyInterpreter()
{
  close();
}

int PixyInterpreter::init()
{
  int USB_return_value;

  if(thread_dead_ == false) 
  {
    fprintf(stderr, "libpixy: Already initialized.");
    return 0;
  }

  USB_return_value = link_.open();

  if(USB_return_value < 0) {
    return USB_return_value;
  }

  receiver_ = new ChirpReceiver(&link_, this);

  // Create the interpreter thread //

  thread_dead_ = false;
  thread_      = boost::thread(&PixyInterpreter::interpreter_thread, this);

  return 0;
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
  
  if (receiver_)  
  {
    delete receiver_;
    receiver_ = NULL;
  }
}

int PixyInterpreter::get_blocks(int max_blocks, Block * blocks)
{
  uint16_t number_of_blocks_to_copy;
  uint16_t index;

  // Check parameters //

  if(max_blocks < 0 || blocks == 0) {
    return PIXY_ERROR_INVALID_PARAMETER;
  }
    
  // Prevent other thread from accessing 'blocks_' while we're using it. //

  blocks_access_mutex_.lock();

  number_of_blocks_to_copy = (max_blocks >= blocks_.size() ? blocks_.size() : max_blocks);
  
  // Copy blocks //
  
  for (index = 0; index != number_of_blocks_to_copy; ++index) {
    memcpy(&blocks[index], &blocks_[index], sizeof(Block));
  }

  blocks_are_new_ = false;
  blocks_access_mutex_.unlock();

  return number_of_blocks_to_copy;
}

int PixyInterpreter::send_command(const char * name, ...)
{
  va_list arguments;
  int     return_value;

  va_start(arguments, name);
  return_value = send_command(name, arguments);
  va_end(arguments);

  return return_value;
}

int PixyInterpreter::send_command(const char * name, va_list args)
{
  ChirpProc procedure_id;
  int       return_value;
  va_list   arguments;

  va_copy(arguments, args);

  // Mutual exclusion for receiver_ object (Lock) //
  chirp_access_mutex_.lock();

  // Request chirp procedure id for 'name'. //
  procedure_id = receiver_->getProc(name);

  // Was there an error requesting procedure id? //
  if (procedure_id < 0) {
    // Request error //
    va_end(arguments);

    // Mutual exclusion for receiver_ object (Unlock) //
    chirp_access_mutex_.unlock();

    return PIXY_ERROR_INVALID_COMMAND;
  }

  // Execute chirp synchronous remote procedure call //
  return_value = receiver_->call(SYNC, procedure_id, arguments); 
  va_end(arguments);

  // Mutual exclusion for receiver_ object (Unlock) //
  chirp_access_mutex_.unlock();

  return return_value;
}

void PixyInterpreter::interpreter_thread()
{
  thread_dead_ = false;
  // Read from Pixy USB connection using the Chirp //
  // protocol until we're told to stop.            //
  while(!thread_die_) {
    // Mutual exclusion for receiver_ object (Lock) //
    chirp_access_mutex_.lock();

    receiver_->service(false);

    // Mutual exclusion for receiver_ object (Unlock) //
    chirp_access_mutex_.unlock();
    usleep(15000); // wait for 15ms, ie give time for 
  }

  thread_dead_ = true;
}


void PixyInterpreter::interpret_data(const void * chirp_data[])
{
  uint8_t  chirp_message;
  uint32_t chirp_type;
  if (chirp_data[0]) {

    chirp_message = Chirp::getType(chirp_data[0]);

    switch(chirp_message) {
      
      case CRP_TYPE_HINT:
        
        chirp_type = * static_cast<const uint32_t *>(chirp_data[0]);

        switch(chirp_type) {

          case FOURCC('B', 'A', '8', '1'):
            break;
          case FOURCC('C', 'C', 'Q', '1'):
            break;
          case FOURCC('C', 'C', 'B', '1'):
            interpret_CCB1(chirp_data + 1);
            break;
          case FOURCC('C', 'C', 'B', '2'):
            interpret_CCB2(chirp_data + 1);
            break;
          case FOURCC('C', 'M', 'V', '1'):
            break;
          default:
            printf("libpixy: Chirp hint [%u] not recognized.\n", chirp_type);
            break;
        }

        break;

      case CRP_HSTRING:

        break;
      
      default:
       
       fprintf(stderr, "libpixy: Unknown message received from Pixy: [%u]\n", chirp_message);
       break;
    }
  } 
}

void PixyInterpreter::interpret_CCB1(const void * CCB1_data[])
{
  uint32_t       number_of_blobs;
  const BlobA *  blobs;
  uint32_t       index;
  
  // Add blocks with normal signatures //
  
  number_of_blobs = * static_cast<const uint32_t *>(CCB1_data[3]);
  blobs           = static_cast<const BlobA *>(CCB1_data[4]);
  
  number_of_blobs /= sizeof(BlobA) / sizeof(uint16_t);
  
  // Wait for permission to use blocks_ vector //
  blocks_access_mutex_.lock();

  add_normal_blocks(blobs, number_of_blobs);
  blocks_are_new_ = true;
  blocks_access_mutex_.unlock();
}


void PixyInterpreter::interpret_CCB2(const void * CCB2_data[])
{
  uint32_t       number_of_blobs;
  const BlobA *  A_blobs;
  const BlobB *  B_blobs;
  uint32_t       index;

  // Wait for permission to use blocks_ vector //
  blocks_access_mutex_.lock();

  // The blocks container will only contain the newest //
  // blocks                                            //
  blocks_.clear();

  // Add blocks with color code signatures //

  number_of_blobs = * static_cast<const uint32_t *>(CCB2_data[5]);
  B_blobs         = static_cast<const BlobB *>(CCB2_data[6]);
  
  number_of_blobs /= sizeof(BlobB) / sizeof(uint16_t);
  add_color_code_blocks(B_blobs, number_of_blobs);

  // Add blocks with normal signatures //

  number_of_blobs = * static_cast<const uint32_t *>(CCB2_data[3]);
  A_blobs         = static_cast<const BlobA *>(CCB2_data[4]);
  
  number_of_blobs /= sizeof(BlobA) / sizeof(uint16_t);
  
  add_normal_blocks(A_blobs, number_of_blobs);
  blocks_are_new_ = true;
  blocks_access_mutex_.unlock();
}

void PixyInterpreter::add_normal_blocks(const BlobA * blocks, uint32_t count)
{
  uint32_t index;
  Block    block;

  for (index = 0; index != count; ++index) {

    // Decode CCB1 'Normal' Signature Type //

    block.type      = PIXY_BLOCKTYPE_NORMAL;
    block.signature = blocks[index].m_model;
    block.width     = blocks[index].m_right - blocks[index].m_left;
    block.height    = blocks[index].m_bottom - blocks[index].m_top;
    block.x         = blocks[index].m_left + block.width / 2;
    block.y         = blocks[index].m_top + block.height / 2;

    // Angle is not a valid parameter for 'Normal'  //
    // signature types. Setting to zero by default. //
    block.angle     = 0;
      
    // Store new block in block buffer //

    if (blocks_.size() == PIXY_BLOCK_CAPACITY) {
      // Blocks buffer is full - replace oldest received block with newest block //
      blocks_.erase(blocks_.begin());
      blocks_.push_back(block);
    } else {
      // Add new block to blocks buffer //
      blocks_.push_back(block);
    }
  }
}

void PixyInterpreter::add_color_code_blocks(const BlobB * blocks, uint32_t count)
{
  uint32_t index;
  Block    block;
    
  for (index = 0; index != count; ++index) {

    // Decode 'Color Code' Signature Type //

    block.type      = PIXY_BLOCKTYPE_COLOR_CODE;
    block.signature = blocks[index].m_model;
    block.width     = blocks[index].m_right - blocks[index].m_left;
    block.height    = blocks[index].m_bottom - blocks[index].m_top;
    block.x         = blocks[index].m_left + block.width / 2;
    block.y         = blocks[index].m_top + block.height / 2;
    block.angle     = blocks[index].m_angle;

    // Store new block in block buffer //

    if (blocks_.size() == PIXY_BLOCK_CAPACITY) {
      // Blocks buffer is full - replace oldest received block with newest block //
      blocks_.erase(blocks_.begin());
      blocks_.push_back(block);
    } else {
      // Add new block to blocks buffer //
      blocks_.push_back(block);
    }
  }
}

int PixyInterpreter::blocks_are_new()
{
  usleep(100); // sleep a bit so client doesn't need to
  if (blocks_are_new_) {
    // Fresh blocks!! :D //
    return 1;
  } else {
    // Stale blocks... :\ //
    return 0;
  }
}
