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

/*----------------------------------------------------------------------------
 *      U S B  -  K e r n e l
 *----------------------------------------------------------------------------
 * Name:    usbdesc.c
 * Purpose: USB Descriptors
 * Version: V1.20
 *----------------------------------------------------------------------------
 *      This software is supplied "AS IS" without any warranties, express,
 *      implied or statutory, including but not limited to the implied
 *      warranties of fitness for purpose, satisfactory quality and
 *      noninfringement. Keil extends you a royalty-free right to reproduce
 *      and distribute executable files created using this software for use
 *      on NXP Semiconductors LPC microcontroller devices only. Nothing else 
 *      gives you the right to use this software.
 *
 * Copyright (c) 2009 Keil - An ARM Company. All rights reserved.
 *----------------------------------------------------------------------------
 * History:
 *          V1.20 Changed string descriptor handling
 *          V1.00 Initial Version
 *----------------------------------------------------------------------------*/
#include "lpc43xx.h"
#include "lpc_types.h"

#include "usb.h"
#include "usbcfg.h"
#include "usbdesc.h"

#define ALIGNED(n)  __attribute__((aligned (n)))

#ifdef __cplusplus
 extern "C" {
#endif


/* USB Standard Device Descriptor */
 ALIGNED(4) const uint8_t USB_DeviceDescriptor[] = {
  USB_DEVICE_DESC_SIZE,              /* bLength */
  USB_DEVICE_DESCRIPTOR_TYPE,        /* bDescriptorType */
  WBVAL(0x0200), /* 2.0 */           /* bcdUSB */
  0xff,                              /* bDeviceClass */
  0x01,                              /* bDeviceSubClass */
  0x00,                              /* bDeviceProtocol */
  USB_MAX_PACKET0,                   /* bMaxPacketSize0 */
  WBVAL(0xb1ac),                     /* idVendor */
  WBVAL(0xf000),                     /* idProduct */
  WBVAL(0x0100), /* 1.00 */          /* bcdDevice */
  0x01,                              /* iManufacturer */
  0x02,                              /* iProduct */
  0x03,                              /* iSerialNumber */
  0x01                               /* bNumConfigurations: one possible configuration*/
};

/* USB FSConfiguration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) const uint8_t USB_FSConfigDescriptor[] = {
/* Configuration 1 */
  USB_CONFIGUARTION_DESC_SIZE,       /* bLength */
  USB_CONFIGURATION_DESCRIPTOR_TYPE, /* bDescriptorType */
  WBVAL(                             /* wTotalLength */
    (1 * USB_CONFIGUARTION_DESC_SIZE) +
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* communication interface */
    (1 * USB_ENDPOINT_DESC_SIZE)      +  /* interrupt endpoint */
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* data interface */
    (2 * USB_ENDPOINT_DESC_SIZE)         /* bulk endpoints */
      ),
  0x02,                              /* bNumInterfaces */
  0x01,                              /* bConfigurationValue: 0x01 is used to select this configuration */
  0x00,                              /* iConfiguration: no string to describe this configuration */
  USB_CONFIG_BUS_POWERED /*|*/       /* bmAttributes */
/*USB_CONFIG_REMOTE_WAKEUP*/,
  USB_CONFIG_POWER_MA(100),          /* bMaxPower, device power consumption is 100 mA */
/* Interface 0, Alternate Setting 0, Communication class interface descriptor */
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x00,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: Alternate setting */
  0x01,                              /* bNumEndpoints: One endpoint used */
  0xff,                              /* bInterfaceClass:  */
  0x01,                              /* bInterfaceSubClass: Abstract Control Model */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/*Endpoint 1 Descriptor*/            /* event notification (optional) */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(1),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_INTERRUPT,       /* bmAttributes */
  WBVAL(0x0010),                     /* wMaxPacketSize */
  0x02,          /* 2ms */           /* bInterval */
/* Interface 1, Alternate Setting 0, Data class interface descriptor*/
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x01,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: no alternate setting */
  0x02,                              /* bNumEndpoints: two endpoints used */
  0xff,                              /* bInterfaceClass: Data Interface Class */
  0x01,                              /* bInterfaceSubClass: no subclass available */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/* Endpoint, EP2 Bulk Out */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_OUT(2),               /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Endpoint, EP2 Bulk In */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(2),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Terminator */
  0                                  /* bLength */
                           /* bLength */
};

/* USB HSConfiguration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) const uint8_t USB_HSConfigDescriptor[] = {
/* Configuration 1 */
  USB_CONFIGUARTION_DESC_SIZE,       /* bLength */
  USB_CONFIGURATION_DESCRIPTOR_TYPE, /* bDescriptorType */
  WBVAL(                             /* wTotalLength */
    (1 * USB_CONFIGUARTION_DESC_SIZE) +
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* communication interface */
    (1 * USB_ENDPOINT_DESC_SIZE)      +  /* interrupt endpoint */
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* data interface */
    (2 * USB_ENDPOINT_DESC_SIZE)         /* bulk endpoints */
      ),
  0x02,                              /* bNumInterfaces */
  0x01,                              /* bConfigurationValue: 0x01 is used to select this configuration */
  0x00,                              /* iConfiguration: no string to describe this configuration */
  USB_CONFIG_BUS_POWERED /*|*/       /* bmAttributes */
/*USB_CONFIG_REMOTE_WAKEUP*/,
  USB_CONFIG_POWER_MA(100),          /* bMaxPower, device power consumption is 100 mA */
/* Interface 0, Alternate Setting 0, Communication class interface descriptor */
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x00,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: Alternate setting */
  0x01,                              /* bNumEndpoints: One endpoint used */
  0xff,                              /* bInterfaceClass: */
  0x01,                              /* bInterfaceSubClass: */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/*Endpoint 1 Descriptor*/            /* event notification (optional) */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(1),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_INTERRUPT,       /* bmAttributes */
  WBVAL(0x0010),                     /* wMaxPacketSize */
  0x02,          /* 2ms */           /* bInterval */
/* Interface 1, Alternate Setting 0, Data class interface descriptor*/
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x01,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: no alternate setting */
  0x02,                              /* bNumEndpoints: two endpoints used */
  0xff,                              /* bInterfaceClass: */
  0x01,                              /* bInterfaceSubClass: no subclass available */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/* Endpoint, EP2 Bulk Out */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_OUT(2),               /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(512),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Endpoint, EP2 Bulk In */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(2),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(512),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Terminator */
  0                                  /* bLength */
                              /* bLength */
};

/* USB String Descriptor (optional) */
ALIGNED(4) const uint8_t USB_StringDescriptor[] = {
/* Index 0x00: LANGID Codes */
  0x04,                              /* bLength */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  WBVAL(0x0409), /* US English */    /* wLANGID */
/* Index 0x01: Manufacturer */
  (12*2 + 2),                        /* bLength (13 Char + Type + lenght) */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'C',0,
  'h',0,
  'a',0,
  'r',0,
  'm',0,
  'e',0,
  'd',0,
  ' ',0,
  'L',0,
  'a',0,
  'b',0,
  's',0,
/* Index 0x02: Product */
  (4*2 + 2),                        /* bLength ( 17 Char + Type + lenght) */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'P',0,
  'i',0,
  'x',0,
  'y',0,
/* Index 0x03: Serial Number */
  (8*2 + 2),                        /* bLength (12 Char + Type + lenght) */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'D',0,
  'E',0,
  'M',0,
  'O',0,
  ' ',0,
  '0',0,
  '.',0,
  '0',0,
/* Index 0x04: Interface 0, Alternate Setting 0 */
  ( 6*2 + 2),                        /* bLength (4 Char + Type + lenght) */
  USB_STRING_DESCRIPTOR_TYPE,        /* bDescriptorType */
  'C',0,
  'a',0,
  'm',0,
  'e',0,
  'r',0,
  'a',0,
};

/* USB Device Qualifier */
ALIGNED(4) const uint8_t USB_DeviceQualifier[] = {
  USB_DEVICE_QUALI_SIZE,             	/* bLength */
  USB_DEVICE_QUALIFIER_DESCRIPTOR_TYPE,	/* bDescriptorType */
  WBVAL(0x0200), /* 2.00 */          /* bcdUSB */
  0x00,                              /* bDeviceClass */
  0x00,                              /* bDeviceSubClass */
  0x00,                              /* bDeviceProtocol */
  USB_MAX_PACKET0,                   /* bMaxPacketSize0 */
  0x01,                              /* bNumOtherSpeedConfigurations */
  0x00                               /* bReserved */
};

/* USB Configuration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) const uint8_t USB_FSOtherSpeedConfiguration[] = {
/* Configuration 1 */
  USB_CONFIGUARTION_DESC_SIZE,       /* bLength */
  USB_CONFIGURATION_DESCRIPTOR_TYPE, /* bDescriptorType */
  WBVAL(                             /* wTotalLength */
    (1 * USB_CONFIGUARTION_DESC_SIZE) +
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* communication interface */
    (1 * USB_ENDPOINT_DESC_SIZE)      +  /* interrupt endpoint */
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* data interface */
    (2 * USB_ENDPOINT_DESC_SIZE)         /* bulk endpoints */
      ),
  0x02,                              /* bNumInterfaces */
  0x01,                              /* bConfigurationValue: 0x01 is used to select this configuration */
  0x00,                              /* iConfiguration: no string to describe this configuration */
  USB_CONFIG_BUS_POWERED /*|*/       /* bmAttributes */
/*USB_CONFIG_REMOTE_WAKEUP*/,
  USB_CONFIG_POWER_MA(100),          /* bMaxPower, device power consumption is 100 mA */
/* Interface 0, Alternate Setting 0, Communication class interface descriptor */
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x00,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: Alternate setting */
  0x01,                              /* bNumEndpoints: One endpoint used */
  0xff,                              /* bInterfaceClass: Communication Interface Class */
  0x01,                              /* bInterfaceSubClass: Abstract Control Model */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/*Endpoint 1 Descriptor*/            /* event notification (optional) */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(1),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_INTERRUPT,       /* bmAttributes */
  WBVAL(0x0010),                     /* wMaxPacketSize */
  0x02,          /* 2ms */           /* bInterval */
/* Interface 1, Alternate Setting 0, Data class interface descriptor*/
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x01,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: no alternate setting */
  0x02,                              /* bNumEndpoints: two endpoints used */
  0xff,                              /* bInterfaceClass: Data Interface Class */
  0x01,                              /* bInterfaceSubClass: no subclass available */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/* Endpoint, EP2 Bulk Out */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_OUT(2),               /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Endpoint, EP2 Bulk In */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(2),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Terminator */
  0                                  /* bLength */
};

/* USB Configuration Descriptor */
/*   All Descriptors (Configuration, Interface, Endpoint, Class, Vendor */
ALIGNED(4) const uint8_t USB_HSOtherSpeedConfiguration[] = {
/* Configuration 1 */
  USB_CONFIGUARTION_DESC_SIZE,       /* bLength */
  USB_CONFIGURATION_DESCRIPTOR_TYPE, /* bDescriptorType */
  WBVAL(                             /* wTotalLength */
    (1 * USB_CONFIGUARTION_DESC_SIZE) +
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* communication interface */
    (1 * USB_ENDPOINT_DESC_SIZE)      +  /* interrupt endpoint */
    (1 * USB_INTERFACE_DESC_SIZE)     +  /* data interface */
    (2 * USB_ENDPOINT_DESC_SIZE)         /* bulk endpoints */
      ),
  0x02,                              /* bNumInterfaces */
  0x01,                              /* bConfigurationValue: 0x01 is used to select this configuration */
  0x00,                              /* iConfiguration: no string to describe this configuration */
  USB_CONFIG_BUS_POWERED /*|*/       /* bmAttributes */
/*USB_CONFIG_REMOTE_WAKEUP*/,
  USB_CONFIG_POWER_MA(100),          /* bMaxPower, device power consumption is 100 mA */
/* Interface 0, Alternate Setting 0, Communication class interface descriptor */
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x00,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: Alternate setting */
  0x01,                              /* bNumEndpoints: One endpoint used */
  0xff,                              /* bInterfaceClass: Communication Interface Class */
  0x01,                              /* bInterfaceSubClass: Abstract Control Model */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/*Endpoint 1 Descriptor*/            /* event notification (optional) */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(1),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_INTERRUPT,       /* bmAttributes */
  WBVAL(0x0010),                     /* wMaxPacketSize */
  0x02,          /* 2ms */           /* bInterval */
/* Interface 1, Alternate Setting 0, Data class interface descriptor*/
  USB_INTERFACE_DESC_SIZE,           /* bLength */
  USB_INTERFACE_DESCRIPTOR_TYPE,     /* bDescriptorType */
  0x01,                              /* bInterfaceNumber: Number of Interface */
  0x00,                              /* bAlternateSetting: no alternate setting */
  0x02,                              /* bNumEndpoints: two endpoints used */
  0xff,                              /* bInterfaceClass: Data Interface Class */
  0x01,                              /* bInterfaceSubClass: no subclass available */
  0x00,                              /* bInterfaceProtocol: no protocol used */
  0x04,                              /* iInterface: */
/* Endpoint, EP2 Bulk Out */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_OUT(2),               /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Endpoint, EP2 Bulk In */
  USB_ENDPOINT_DESC_SIZE,            /* bLength */
  USB_ENDPOINT_DESCRIPTOR_TYPE,      /* bDescriptorType */
  USB_ENDPOINT_IN(2),                /* bEndpointAddress */
  USB_ENDPOINT_TYPE_BULK,            /* bmAttributes */
  WBVAL(USB_DEV_BUFSIZE),            /* wMaxPacketSize */
  0x00,                              /* bInterval: ignore for Bulk transfer */
/* Terminator */
  0                                  /* bLength */
};

#ifdef __cplusplus
 }
#endif
