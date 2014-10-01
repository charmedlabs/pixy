#!/usr/bin/env python

from distutils.core import setup, Extension

pixy_module = Extension('_pixy', 
	include_dirs = ['/usr/include/libusb-1.0',
	'../../common/',
	'../libpixyusb/src/utils'],
	libraries = ['boost_thread',
	'boost_system',
	'boost_chrono',
	'pthread',
	'usb-1.0'],
	sources=['pixy_wrap.cxx', 
	'../../common/chirp.cpp',
	'../libpixyusb/src/pixy.cpp',
	'../libpixyusb/src/chirpreceiver.cpp',
	'../libpixyusb/src/pixyinterpreter.cpp',
	'../libpixyusb/src/usblink.cpp',
	'../libpixyusb/src/utils/timer.cpp'])

setup (name = 'pixy',
	version = '0.3',
	author  = 'Charmed Labs, LLC',
	description = """libpixyusb module""",
	ext_modules = [pixy_module],
	py_modules = ["pixy"],
	)
