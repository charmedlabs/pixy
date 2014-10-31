#
# begin license header
#
# This file is part of Pixy CMUcam5 or "Pixy" for short
#
# All Pixy source code is provided under the terms of the
# GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
# Those wishing to use Pixy source code, software and/or
# technologies under different licensing terms should contact us at
# cmucam@cs.cmu.edu. Such licensing terms are available for
# all portions of the Pixy codebase presented here.
#
# end license header
#

if (LIBPIXYUSB_0_LIBRARY AND LIBPIXYUSB_0_INCLUDE_DIR)
  # In cmake cache already
  set (LIBPIXYUSB_0_FOUND TRUE)
else (LIBPIXYUSB_0_LIBRARY AND LIBPIXYUSB_0_INCLUDE_DIR)
  find_path(INCLUDE_DIR
    NAMES
      pixy.h
      pixy_defs.h
    PATHS
      /usr/include
      /usr/local/include
      /opt/local/include
      /sw/include
  )

  find_library(LIBRARY_DIR
    NAMES
      pixyusb
    PATHS
      /usr/lib
      /usr/local/lib
      /opt/local/lib
      /sw/lib
  )

  message ("lib ${LIBRARY_DIR}")
  message ("inc ${INCLUDE_DIR}")

  set (LIBPIXYUSB_0_INCLUDE_DIR ${INCLUDE_DIR})
  set (LIBPIXYUSB_0_LIBRARY ${LIBRARY_DIR})

  if (LIBPIXYUSB_0_INCLUDE_DIR AND LIBPIXYUSB_0_LIBRARY)
    set (LIBPIXYUSB_0_FOUND TRUE)
  endif (LIBPIXYUSB_0_INCLUDE_DIR AND LIBPIXYUSB_0_LIBRARY)

  if (LIBPIXYUSB_0_FOUND)
    message (STATUS "Found libpixyusb")
  else (LIBPIXYUSB_0_FOUND)
    message (FATAL_ERROR "Could not find libpixyusb")
  endif (LIBPIXYUSB_0_FOUND)

  mark_as_advanced (LIBPIXYUSB_0_INCLUDE_DIR LIBPIXYUSB_0_LIBRARY)
      
endif (LIBPIXYUSB_0_LIBRARY AND LIBPIXYUSB_0_INCLUDE_DIR)
