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

#ifndef __PIXYINTERPRETER_HPP__
#define __PIXYINTERPRETER_HPP__

#include <vector>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include "pixytypes.h"
#include "pixy.h"
#include "usblink.h"
#include "interpreter.hpp"
#include "chirpreceiver.hpp"

#define PIXY_BLOCK_CAPACITY         250

class PixyInterpreter : public Interpreter
{
  public:

    PixyInterpreter();

    /**
      @brief  Spawns an 'interpreter' thread which attempts to 
              connect to Pixy using the USB interface. 
              On successful connection, this thread will 
              capture and store Pixy 'block' object data 
              which can be retreived using the getBlocks()
              method.
       @return   0    Success
       @return  -1    Error: Unable to open pixy USB device

    */
    int init();
    
    /**
      @brief  Terminates the USB connection to Pixy and
              the 'iterpreter' thread.
    */
    void close();

    /**
      @brief      Copies up to 'max_blocks' number of Blocks to the address pointed
                  to by 'blocks'. 
      @param[in]  max_blocks Maximum number of Blocks to copy to the address pointed to
                             by 'blocks'.
      @param[out] blocks     Address of an array in which to copy the blocks to.
                             The array must be large enough to write 'max_blocks' number
                             of Blocks to.
      @return     Number of blocks copied.
      */
      uint16_t get_blocks(uint16_t max_blocks, Block * blocks);

      /** 
        @brief         Sends a command to Pixy. 
        @param[in]     name       Remote procedure call identifier string.
        @param[in,out] arguments  Argument list to function call.
        @return        -1         Error
      */
      int send_command(const char * name, va_list arguments);
      
      /** 
        @brief         Sends a command to Pixy. 
        @param[in]     name       Remote procedure call identifier string.
        @return        -1         Error
      */
      int send_command(const char * name, ...);

  private:
    
    ChirpReceiver *    receiver_;
    USBLink            link_;
    boost::thread      thread_;
    bool               thread_die_;
    bool               thread_dead_;
    std::vector<Block> blocks_;
    boost::mutex       blocks_access_mutex_;
    boost::mutex       chirp_access_mutex_;

    /**
      @brief  Interpreter thread entry point. 
       
              Performs the following operations:

              1. Connect to Pixy.
              2. Interpretes Pixy messages and saves
                 pixy 'block' objects.
    */
    void interpreter_thread(); 

    /**
      @brief Interprets data sent from Pixy over the Chirp protocol.

      @param[in] data  Incoming Chirp protocol data from Pixy.
    */
    void interpret_data(void * chrip_data[]);

    /**
      @brief Interprets CCB1 messages sent from Pixy.

      @param[in] data  Incoming Chirp protocol data from Pixy.
    */
    void interpret_CCB1(void * data[]);
    
    /**
      @brief Interprets CCB2 messages sent from Pixy.

      @param[in] data  Incoming Chirp protocol data from Pixy.
    */
    void interpret_CCB2(void * data[]);

    /**
      @brief Adds blocks with normal signatures to the PixyInterpreter
             'blocks_' buffer.

      @param[in] blocks  An array of normal signature blocks to add to buffer.
      @param[in] count   Size of the 'blocks' array.
    */
    void add_normal_blocks(BlobA * blocks, uint32_t count);
    
    /**
      @brief Adds blocks with color code signatures to the PixyInterpreter
             'blocks_' buffer.

      @param[in] blocks  An array of color code signature blocks to add to buffer.
      @param[in] count   Size of the 'blocks' array.
    */
    void add_color_code_blocks(BlobB * blocks, uint32_t count);
};

#endif
