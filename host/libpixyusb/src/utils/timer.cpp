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

#ifdef __USING_CXX11__
#include <chrono>
#elif  __USING_BOOST__
#else
#error C++11 or boost libraries required for mutex support.
#endif

#include "timer.hpp"

#ifdef __USING_CXX11__
using namespace std::chrono;
#endif

std::string util::timer::implementation()
{
  #ifdef __USING_CXX11__
  return std::string("C++11");
  #elif  __USING_BOOST__
  return std::string("BOOST"); 
  #endif
}

util::timer::timer()
{
  #ifdef __USING_CXX11__
  epoch_ = steady_clock::now();
  #elif  __USING_BOOST__
  #endif
}

void util::timer::reset()
{
  #ifdef __USING_CXX11__
  epoch_ = steady_clock::now();
  #elif  __USING_BOOST__
  #endif
}

uint32_t util::timer::elapsed()
{
  #ifdef __USING_CXX11__
  steady_clock::time_point mark;
  
  // Compute difference in time //
  
  mark = steady_clock::now();
  return duration_cast<milliseconds>(mark - epoch_).count();
  #elif  __USING_BOOST__
  #endif
}


