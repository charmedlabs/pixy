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
#ifdef __USING_CXX11__
#include <thread>
#elif  __USING_BOOST__
#include <boost/thread.hpp>
#endif
#include "pixy.h"
#include "usblink.h"
#include "utils/mutex.hpp"
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
    */
    void init();
    
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

  private:
    
    ChirpReceiver *    receiver_;
    USBLink            link_;
    #ifdef __USING_CXX11__
    std::thread        thread_;
    #elif  __USING_BOOST__
    boost::thread      thread_;
    #endif
    bool               thread_die_;
    bool               thread_dead_;
    std::vector<Block> blocks_;
    util::mutex        blocks_access_mutex_;

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
      @brief Interprets CCB1 'block' objects sent from Pixy.

      @param[in] data  Incoming Chirp protocol data from Pixy.
    */
    void interpret_CCB1(void * CCB1_data[]);
};

#endif
