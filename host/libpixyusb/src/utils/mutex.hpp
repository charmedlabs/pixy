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

#ifndef __MUTEX_HPP__
#define __MUTEX_HPP__

#include <string>

#ifdef __USING_CXX11__
#include <mutex>
#elif  __USING_BOOST__
#include <boost/thread/mutex.hpp>
#else
#error CXX11 or boost libraries required for mutex support.
#endif

namespace util
{
  class mutex
  {
    public:

      void lock();
      void unlock();

      std::string implementation();

    private:

      #ifdef __USING_CXX11__
      std::mutex   mutex_;
      #elif  __USING_BOOST__
      boost::mutex mutex_;
      #endif
  };
}

#endif
