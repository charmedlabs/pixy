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

#ifndef __CHIRPRECEIVER_HPP__
#define __CHIRPRECEIVER_HPP__

#include "chirp.hpp"
#include "usblink.h"
#include "interpreter.hpp"

class ChirpReceiver : public Chirp
{
  public:

    ChirpReceiver(USBLink * link, Interpreter * interpreter);

  private:
    
    Interpreter * interpreter_;

    /**
      @brief Called by Chrip::service() when data
             is received from Pixy. 

      @param[in] data  Incoming Chirp protocol data from Pixy.
    */
    void handleXdata(void * data[]);
};

#endif
