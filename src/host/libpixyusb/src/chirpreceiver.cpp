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

#include "chirpreceiver.hpp"

ChirpReceiver::ChirpReceiver(USBLink * link, Interpreter * interpreter)
{
  m_hinterested = true;
  m_client      = true;
  interpreter_  = interpreter;

  setLink(link);
}

ChirpReceiver::~ChirpReceiver()
{
  // This destructor does nothing but is necessary  //
  // for successful linkage on some combinations of //
  // compilers and platforms.                       //
}

void ChirpReceiver::handleXdata(const void * data[])
{
  // Interpret (Chirp) messages from Pixy //
  interpreter_->interpret_data(data);
}
