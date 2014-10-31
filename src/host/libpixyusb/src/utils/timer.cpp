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

#include "timer.hpp"

using namespace boost::chrono;

util::timer::timer()
{
  epoch_ = steady_clock::now();
}

void util::timer::reset()
{
  epoch_ = steady_clock::now();
}

uint32_t util::timer::elapsed()
{
  steady_clock::time_point mark;
  
  // Compute difference in time //
  
  mark = steady_clock::now();
  return duration_cast<milliseconds>(mark - epoch_).count();
}
