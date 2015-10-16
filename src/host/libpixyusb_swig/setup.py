#!/usr/bin/env python

from distutils.core import setup, Extension

# The paths used in this file are relative to: PIXY_ROOT/build #

pixy_module = Extension('_pixy', 
	include_dirs = ['/usr/include/libusb-1.0',
  '/usr/local/include/libusb-1.0',
	'../../src/common/inc',
	'../../src/host/libpixyusb/src/utils',
  '../../src/host/libpixyusb/include'],
	libraries = ['boost_thread',
	'boost_system',
	'boost_chrono',
	'pthread',
	'usb-1.0'],
	sources=['pixy_wrap.cxx', 
	'../../src/common/src/chirp.cpp',
	'../../src/host/libpixyusb/src/pixy.cpp',
	'../../src/host/libpixyusb/src/chirpreceiver.cpp',
	'../../src/host/libpixyusb/src/pixyinterpreter.cpp',
	'../../src/host/libpixyusb/src/usblink.cpp',
	'../../src/host/libpixyusb/src/utils/timer.cpp'])

setup (name = 'pixy',
	version = '0.3',
	author  = 'Charmed Labs, LLC',
	description = """libpixyusb module""",
	ext_modules = [pixy_module],
	py_modules = ["pixy"],
	)
