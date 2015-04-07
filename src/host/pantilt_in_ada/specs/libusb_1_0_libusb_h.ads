--
--  Copyright (c) 2015, John Leimon <jleimon@gmail.com>
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above copyright
--  notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
--  TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN
--  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
--  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
--  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
--  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stdint_h;
with Interfaces.C.Strings;
with System;
with stdio_h;
limited with x86_64_linux_gnu_bits_time_h;

package libusb_1_0_libusb_h is

   --  unsupported macro: LIBUSB_DEPRECATED_FOR(f) __attribute__((deprecated("Use " #f " instead")))
   --  unsupported macro: LIBUSBX_API_VERSION 0x01000102
   --  unsupported macro: libusb_le16_to_cpu libusb_cpu_to_le16
   --  unsupported macro: LIBUSB_DT_DEVICE_SIZE 18
   --  unsupported macro: LIBUSB_DT_CONFIG_SIZE 9
   --  unsupported macro: LIBUSB_DT_INTERFACE_SIZE 9
   --  unsupported macro: LIBUSB_DT_ENDPOINT_SIZE 7
   --  unsupported macro: LIBUSB_DT_ENDPOINT_AUDIO_SIZE 9
   --  unsupported macro: LIBUSB_DT_HUB_NONVAR_SIZE 7
   --  unsupported macro: LIBUSB_DT_SS_ENDPOINT_COMPANION_SIZE 6
   --  unsupported macro: LIBUSB_DT_BOS_SIZE 5
   --  unsupported macro: LIBUSB_DT_DEVICE_CAPABILITY_SIZE 3
   --  unsupported macro: LIBUSB_BT_USB_2_0_EXTENSION_SIZE 7
   --  unsupported macro: LIBUSB_BT_SS_USB_DEVICE_CAPABILITY_SIZE 10
   --  unsupported macro: LIBUSB_BT_CONTAINER_ID_SIZE 20
   --  unsupported macro: LIBUSB_DT_BOS_MAX_SIZE ((LIBUSB_DT_BOS_SIZE) + (LIBUSB_BT_USB_2_0_EXTENSION_SIZE) + (LIBUSB_BT_SS_USB_DEVICE_CAPABILITY_SIZE) + (LIBUSB_BT_CONTAINER_ID_SIZE))
   --  unsupported macro: LIBUSB_ENDPOINT_ADDRESS_MASK 0x0f
   --  unsupported macro: LIBUSB_ENDPOINT_DIR_MASK 0x80
   --  unsupported macro: LIBUSB_TRANSFER_TYPE_MASK 0x03
   --  unsupported macro: LIBUSB_ISO_SYNC_TYPE_MASK 0x0C
   --  unsupported macro: LIBUSB_ISO_USAGE_TYPE_MASK 0x30
   --  unsupported macro: LIBUSB_CONTROL_SETUP_SIZE (sizeof(struct libusb_control_setup))
   --  unsupported macro: LIBUSB_ERROR_COUNT 14
   --  unsupported macro: LIBUSB_HOTPLUG_MATCH_ANY -1
   function libusb_cpu_to_le16 (x : stdint_h.uint16_t) return stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:161
   pragma Import (C, libusb_cpu_to_le16, "libusb_cpu_to_le16");

   subtype libusb_class_code is unsigned;
   LIBUSB_CLASS_PER_INTERFACE : constant libusb_class_code := 0;
   LIBUSB_CLASS_AUDIO : constant libusb_class_code := 1;
   LIBUSB_CLASS_COMM : constant libusb_class_code := 2;
   LIBUSB_CLASS_HID : constant libusb_class_code := 3;
   LIBUSB_CLASS_PHYSICAL : constant libusb_class_code := 5;
   LIBUSB_CLASS_PRINTER : constant libusb_class_code := 7;
   LIBUSB_CLASS_PTP : constant libusb_class_code := 6;
   LIBUSB_CLASS_IMAGE : constant libusb_class_code := 6;
   LIBUSB_CLASS_MASS_STORAGE : constant libusb_class_code := 8;
   LIBUSB_CLASS_HUB : constant libusb_class_code := 9;
   LIBUSB_CLASS_DATA : constant libusb_class_code := 10;
   LIBUSB_CLASS_SMART_CARD : constant libusb_class_code := 11;
   LIBUSB_CLASS_CONTENT_SECURITY : constant libusb_class_code := 13;
   LIBUSB_CLASS_VIDEO : constant libusb_class_code := 14;
   LIBUSB_CLASS_PERSONAL_HEALTHCARE : constant libusb_class_code := 15;
   LIBUSB_CLASS_DIAGNOSTIC_DEVICE : constant libusb_class_code := 220;
   LIBUSB_CLASS_WIRELESS : constant libusb_class_code := 224;
   LIBUSB_CLASS_APPLICATION : constant libusb_class_code := 254;
   LIBUSB_CLASS_VENDOR_SPEC : constant libusb_class_code := 255;  -- /usr/include/libusb-1.0/libusb.h:186

   subtype libusb_descriptor_type is unsigned;
   LIBUSB_DT_DEVICE : constant libusb_descriptor_type := 1;
   LIBUSB_DT_CONFIG : constant libusb_descriptor_type := 2;
   LIBUSB_DT_STRING : constant libusb_descriptor_type := 3;
   LIBUSB_DT_INTERFACE : constant libusb_descriptor_type := 4;
   LIBUSB_DT_ENDPOINT : constant libusb_descriptor_type := 5;
   LIBUSB_DT_BOS : constant libusb_descriptor_type := 15;
   LIBUSB_DT_DEVICE_CAPABILITY : constant libusb_descriptor_type := 16;
   LIBUSB_DT_HID : constant libusb_descriptor_type := 33;
   LIBUSB_DT_REPORT : constant libusb_descriptor_type := 34;
   LIBUSB_DT_PHYSICAL : constant libusb_descriptor_type := 35;
   LIBUSB_DT_HUB : constant libusb_descriptor_type := 41;
   LIBUSB_DT_SUPERSPEED_HUB : constant libusb_descriptor_type := 42;
   LIBUSB_DT_SS_ENDPOINT_COMPANION : constant libusb_descriptor_type := 48;  -- /usr/include/libusb-1.0/libusb.h:248

   subtype libusb_endpoint_direction is unsigned;
   LIBUSB_ENDPOINT_IN : constant libusb_endpoint_direction := 128;
   LIBUSB_ENDPOINT_OUT : constant libusb_endpoint_direction := 0;  -- /usr/include/libusb-1.0/libusb.h:318

   type libusb_transfer_type is 
     (LIBUSB_TRANSFER_TYPE_CONTROL,
      LIBUSB_TRANSFER_TYPE_ISOCHRONOUS,
      LIBUSB_TRANSFER_TYPE_BULK,
      LIBUSB_TRANSFER_TYPE_INTERRUPT);
   pragma Convention (C, libusb_transfer_type);  -- /usr/include/libusb-1.0/libusb.h:332

   subtype libusb_standard_request is unsigned;
   LIBUSB_REQUEST_GET_STATUS : constant libusb_standard_request := 0;
   LIBUSB_REQUEST_CLEAR_FEATURE : constant libusb_standard_request := 1;
   LIBUSB_REQUEST_SET_FEATURE : constant libusb_standard_request := 3;
   LIBUSB_REQUEST_SET_ADDRESS : constant libusb_standard_request := 5;
   LIBUSB_REQUEST_GET_DESCRIPTOR : constant libusb_standard_request := 6;
   LIBUSB_REQUEST_SET_DESCRIPTOR : constant libusb_standard_request := 7;
   LIBUSB_REQUEST_GET_CONFIGURATION : constant libusb_standard_request := 8;
   LIBUSB_REQUEST_SET_CONFIGURATION : constant libusb_standard_request := 9;
   LIBUSB_REQUEST_GET_INTERFACE : constant libusb_standard_request := 10;
   LIBUSB_REQUEST_SET_INTERFACE : constant libusb_standard_request := 11;
   LIBUSB_REQUEST_SYNCH_FRAME : constant libusb_standard_request := 12;
   LIBUSB_REQUEST_SET_SEL : constant libusb_standard_request := 48;
   LIBUSB_SET_ISOCH_DELAY : constant libusb_standard_request := 49;  -- /usr/include/libusb-1.0/libusb.h:348

   subtype libusb_request_type is unsigned;
   LIBUSB_REQUEST_TYPE_STANDARD : constant libusb_request_type := 0;
   LIBUSB_REQUEST_TYPE_CLASS : constant libusb_request_type := 32;
   LIBUSB_REQUEST_TYPE_VENDOR : constant libusb_request_type := 64;
   LIBUSB_REQUEST_TYPE_RESERVED : constant libusb_request_type := 96;  -- /usr/include/libusb-1.0/libusb.h:398

   type libusb_request_recipient is 
     (LIBUSB_RECIPIENT_DEVICE,
      LIBUSB_RECIPIENT_INTERFACE,
      LIBUSB_RECIPIENT_ENDPOINT,
      LIBUSB_RECIPIENT_OTHER);
   pragma Convention (C, libusb_request_recipient);  -- /usr/include/libusb-1.0/libusb.h:416

   type libusb_iso_sync_type is 
     (LIBUSB_ISO_SYNC_TYPE_NONE,
      LIBUSB_ISO_SYNC_TYPE_ASYNC,
      LIBUSB_ISO_SYNC_TYPE_ADAPTIVE,
      LIBUSB_ISO_SYNC_TYPE_SYNC);
   pragma Convention (C, libusb_iso_sync_type);  -- /usr/include/libusb-1.0/libusb.h:437

   type libusb_iso_usage_type is 
     (LIBUSB_ISO_USAGE_TYPE_DATA,
      LIBUSB_ISO_USAGE_TYPE_FEEDBACK,
      LIBUSB_ISO_USAGE_TYPE_IMPLICIT);
   pragma Convention (C, libusb_iso_usage_type);  -- /usr/include/libusb-1.0/libusb.h:458

   type libusb_device_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:476
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:481
      bcdUSB : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:485
      bDeviceClass : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:488
      bDeviceSubClass : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:492
      bDeviceProtocol : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:496
      bMaxPacketSize0 : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:499
      idVendor : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:502
      idProduct : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:505
      bcdDevice : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:508
      iManufacturer : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:511
      iProduct : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:514
      iSerialNumber : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:517
      bNumConfigurations : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:520
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_device_descriptor);  -- /usr/include/libusb-1.0/libusb.h:474

   type libusb_endpoint_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:530
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:535
      bEndpointAddress : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:541
      bmAttributes : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:550
      wMaxPacketSize : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:553
      bInterval : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:556
      bRefresh : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:560
      bSynchAddress : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:563
      extra : access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:567
      extra_length : aliased int;  -- /usr/include/libusb-1.0/libusb.h:570
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_endpoint_descriptor);  -- /usr/include/libusb-1.0/libusb.h:528

   type libusb_interface_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:580
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:585
      bInterfaceNumber : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:588
      bAlternateSetting : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:591
      bNumEndpoints : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:595
      bInterfaceClass : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:598
      bInterfaceSubClass : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:602
      bInterfaceProtocol : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:606
      iInterface : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:609
      endpoint : access constant libusb_endpoint_descriptor;  -- /usr/include/libusb-1.0/libusb.h:613
      extra : access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:617
      extra_length : aliased int;  -- /usr/include/libusb-1.0/libusb.h:620
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_interface_descriptor);  -- /usr/include/libusb-1.0/libusb.h:578

   type libusb_interface is record
      altsetting : access constant libusb_interface_descriptor;  -- /usr/include/libusb-1.0/libusb.h:629
      num_altsetting : aliased int;  -- /usr/include/libusb-1.0/libusb.h:632
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_interface);  -- /usr/include/libusb-1.0/libusb.h:626

   type libusb_config_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:642
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:647
      wTotalLength : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:650
      bNumInterfaces : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:653
      bConfigurationValue : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:656
      iConfiguration : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:659
      bmAttributes : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:662
      MaxPower : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:667
      c_interface : access constant libusb_interface;  -- /usr/include/libusb-1.0/libusb.h:671
      extra : access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:675
      extra_length : aliased int;  -- /usr/include/libusb-1.0/libusb.h:678
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_config_descriptor);  -- /usr/include/libusb-1.0/libusb.h:640

   type libusb_ss_endpoint_companion_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:690
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:695
      bMaxBurst : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:700
      bmAttributes : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:706
      wBytesPerInterval : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:710
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_ss_endpoint_companion_descriptor);  -- /usr/include/libusb-1.0/libusb.h:687

   type libusb_bos_dev_capability_descriptor_dev_capability_data_array is array (0 .. -1) of aliased stdint_h.uint8_t;
   type libusb_bos_dev_capability_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:720
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:724
      bDevCapabilityType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:726
      dev_capability_data : aliased libusb_bos_dev_capability_descriptor_dev_capability_data_array;  -- /usr/include/libusb-1.0/libusb.h:732
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_bos_dev_capability_descriptor);  -- /usr/include/libusb-1.0/libusb.h:718

   type libusb_bos_descriptor_dev_capability_array is array (0 .. -1) of access libusb_bos_dev_capability_descriptor;
   type libusb_bos_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:744
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:749
      wTotalLength : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:752
      bNumDeviceCaps : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:756
      dev_capability : aliased libusb_bos_descriptor_dev_capability_array;  -- /usr/include/libusb-1.0/libusb.h:763
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_bos_descriptor);  -- /usr/include/libusb-1.0/libusb.h:742

   type libusb_usb_2_0_extension_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:775
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:780
      bDevCapabilityType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:785
      bmAttributes : aliased stdint_h.uint32_t;  -- /usr/include/libusb-1.0/libusb.h:791
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_usb_2_0_extension_descriptor);  -- /usr/include/libusb-1.0/libusb.h:773

   type libusb_ss_usb_device_capability_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:801
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:806
      bDevCapabilityType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:811
      bmAttributes : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:817
      wSpeedSupported : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:821
      bFunctionalitySupport : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:827
      bU1DevExitLat : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:830
      bU2DevExitLat : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:833
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_ss_usb_device_capability_descriptor);  -- /usr/include/libusb-1.0/libusb.h:799

   type libusb_container_id_descriptor_ContainerID_array is array (0 .. 15) of aliased stdint_h.uint8_t;
   type libusb_container_id_descriptor is record
      bLength : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:843
      bDescriptorType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:848
      bDevCapabilityType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:853
      bReserved : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:856
      ContainerID : aliased libusb_container_id_descriptor_ContainerID_array;  -- /usr/include/libusb-1.0/libusb.h:859
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_container_id_descriptor);  -- /usr/include/libusb-1.0/libusb.h:841

   type libusb_control_setup is record
      bmRequestType : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:870
      bRequest : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:877
      wValue : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:880
      wIndex : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:884
      wLength : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:887
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_control_setup);  -- /usr/include/libusb-1.0/libusb.h:864

   --  skipped empty struct libusb_context

   --  skipped empty struct libusb_device

   --  skipped empty struct libusb_device_handle

   --  skipped empty struct libusb_hotplug_callback

   type libusb_version is record
      major : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:904
      minor : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:907
      micro : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:910
      nano : aliased stdint_h.uint16_t;  -- /usr/include/libusb-1.0/libusb.h:913
      rc : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libusb-1.0/libusb.h:916
      describe : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libusb-1.0/libusb.h:919
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_version);  -- /usr/include/libusb-1.0/libusb.h:902

   type libusb_speed is 
     (LIBUSB_SPEED_UNKNOWN,
      LIBUSB_SPEED_LOW,
      LIBUSB_SPEED_FULL,
      LIBUSB_SPEED_HIGH,
      LIBUSB_SPEED_SUPER);
   pragma Convention (C, libusb_speed);  -- /usr/include/libusb-1.0/libusb.h:972

   subtype libusb_supported_speed is unsigned;
   LIBUSB_LOW_SPEED_OPERATION : constant libusb_supported_speed := 1;
   LIBUSB_FULL_SPEED_OPERATION : constant libusb_supported_speed := 2;
   LIBUSB_HIGH_SPEED_OPERATION : constant libusb_supported_speed := 4;
   LIBUSB_SUPER_SPEED_OPERATION : constant libusb_supported_speed := 8;  -- /usr/include/libusb-1.0/libusb.h:993

   subtype libusb_usb_2_0_extension_attributes is unsigned;
   LIBUSB_BM_LPM_SUPPORT : constant libusb_usb_2_0_extension_attributes := 2;  -- /usr/include/libusb-1.0/libusb.h:1012

   subtype libusb_ss_usb_device_capability_attributes is unsigned;
   LIBUSB_BM_LTM_SUPPORT : constant libusb_ss_usb_device_capability_attributes := 2;  -- /usr/include/libusb-1.0/libusb.h:1022

   subtype libusb_bos_type is unsigned;
   LIBUSB_BT_WIRELESS_USB_DEVICE_CAPABILITY : constant libusb_bos_type := 1;
   LIBUSB_BT_USB_2_0_EXTENSION : constant libusb_bos_type := 2;
   LIBUSB_BT_SS_USB_DEVICE_CAPABILITY : constant libusb_bos_type := 3;
   LIBUSB_BT_CONTAINER_ID : constant libusb_bos_type := 4;  -- /usr/include/libusb-1.0/libusb.h:1030

   subtype libusb_error is unsigned;
   LIBUSB_SUCCESS : constant libusb_error := 0;
   LIBUSB_ERROR_IO : constant libusb_error := -1;
   LIBUSB_ERROR_INVALID_PARAM : constant libusb_error := -2;
   LIBUSB_ERROR_ACCESS : constant libusb_error := -3;
   LIBUSB_ERROR_NO_DEVICE : constant libusb_error := -4;
   LIBUSB_ERROR_NOT_FOUND : constant libusb_error := -5;
   LIBUSB_ERROR_BUSY : constant libusb_error := -6;
   LIBUSB_ERROR_TIMEOUT : constant libusb_error := -7;
   LIBUSB_ERROR_OVERFLOW : constant libusb_error := -8;
   LIBUSB_ERROR_PIPE : constant libusb_error := -9;
   LIBUSB_ERROR_INTERRUPTED : constant libusb_error := -10;
   LIBUSB_ERROR_NO_MEM : constant libusb_error := -11;
   LIBUSB_ERROR_NOT_SUPPORTED : constant libusb_error := -12;
   LIBUSB_ERROR_OTHER : constant libusb_error := -99;  -- /usr/include/libusb-1.0/libusb.h:1051

   type libusb_transfer_status is 
     (LIBUSB_TRANSFER_COMPLETED,
      LIBUSB_TRANSFER_ERROR,
      LIBUSB_TRANSFER_TIMED_OUT,
      LIBUSB_TRANSFER_CANCELLED,
      LIBUSB_TRANSFER_STALL,
      LIBUSB_TRANSFER_NO_DEVICE,
      LIBUSB_TRANSFER_OVERFLOW);
   pragma Convention (C, libusb_transfer_status);  -- /usr/include/libusb-1.0/libusb.h:1103

   subtype libusb_transfer_flags is unsigned;
   LIBUSB_TRANSFER_SHORT_NOT_OK : constant libusb_transfer_flags := 1;
   LIBUSB_TRANSFER_FREE_BUFFER : constant libusb_transfer_flags := 2;
   LIBUSB_TRANSFER_FREE_TRANSFER : constant libusb_transfer_flags := 4;
   LIBUSB_TRANSFER_ADD_ZERO_PACKET : constant libusb_transfer_flags := 8;  -- /usr/include/libusb-1.0/libusb.h:1133

   type libusb_iso_packet_descriptor is record
      length : aliased unsigned;  -- /usr/include/libusb-1.0/libusb.h:1176
      actual_length : aliased unsigned;  -- /usr/include/libusb-1.0/libusb.h:1179
      status : aliased libusb_transfer_status;  -- /usr/include/libusb-1.0/libusb.h:1182
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_iso_packet_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1174

   type libusb_transfer_cb_fn is access procedure (arg1 : System.Address);
   pragma Convention (C, libusb_transfer_cb_fn);  -- /usr/include/libusb-1.0/libusb.h:1196

   type libusb_transfer_iso_packet_desc_array is array (0 .. -1) of aliased libusb_iso_packet_descriptor;
   type libusb_transfer is record
      dev_handle : System.Address;  -- /usr/include/libusb-1.0/libusb.h:1206
      flags : aliased stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:1209
      endpoint : aliased unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1212
      c_type : aliased unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1215
      timeout : aliased unsigned;  -- /usr/include/libusb-1.0/libusb.h:1219
      status : aliased libusb_transfer_status;  -- /usr/include/libusb-1.0/libusb.h:1228
      length : aliased int;  -- /usr/include/libusb-1.0/libusb.h:1231
      actual_length : aliased int;  -- /usr/include/libusb-1.0/libusb.h:1236
      callback : libusb_transfer_cb_fn;  -- /usr/include/libusb-1.0/libusb.h:1240
      user_data : System.Address;  -- /usr/include/libusb-1.0/libusb.h:1243
      buffer : access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1246
      num_iso_packets : aliased int;  -- /usr/include/libusb-1.0/libusb.h:1250
      iso_packet_desc : aliased libusb_transfer_iso_packet_desc_array;  -- /usr/include/libusb-1.0/libusb.h:1257
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_transfer);  -- /usr/include/libusb-1.0/libusb.h:1204

   subtype libusb_capability is unsigned;
   LIBUSB_CAP_HAS_CAPABILITY : constant libusb_capability := 0;
   LIBUSB_CAP_HAS_HOTPLUG : constant libusb_capability := 1;
   LIBUSB_CAP_HAS_HID_ACCESS : constant libusb_capability := 256;
   LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER : constant libusb_capability := 257;  -- /usr/include/libusb-1.0/libusb.h:1267

   type libusb_log_level is 
     (LIBUSB_LOG_LEVEL_NONE,
      LIBUSB_LOG_LEVEL_ERROR,
      LIBUSB_LOG_LEVEL_WARNING,
      LIBUSB_LOG_LEVEL_INFO,
      LIBUSB_LOG_LEVEL_DEBUG);
   pragma Convention (C, libusb_log_level);  -- /usr/include/libusb-1.0/libusb.h:1292

   function libusb_init (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1300
   pragma Import (C, libusb_init, "libusb_init");

   procedure libusb_exit (ctx : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1301
   pragma Import (C, libusb_exit, "libusb_exit");

   procedure libusb_set_debug (ctx : System.Address; level : int);  -- /usr/include/libusb-1.0/libusb.h:1302
   pragma Import (C, libusb_set_debug, "libusb_set_debug");

   function libusb_get_version return access constant libusb_version;  -- /usr/include/libusb-1.0/libusb.h:1303
   pragma Import (C, libusb_get_version, "libusb_get_version");

   function libusb_has_capability (capability : stdint_h.uint32_t) return int;  -- /usr/include/libusb-1.0/libusb.h:1304
   pragma Import (C, libusb_has_capability, "libusb_has_capability");

   function libusb_error_name (errcode : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libusb-1.0/libusb.h:1305
   pragma Import (C, libusb_error_name, "libusb_error_name");

   function libusb_setlocale (locale : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libusb-1.0/libusb.h:1306
   pragma Import (C, libusb_setlocale, "libusb_setlocale");

   function libusb_strerror (errcode : libusb_error) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libusb-1.0/libusb.h:1307
   pragma Import (C, libusb_strerror, "libusb_strerror");

   function libusb_get_device_list (ctx : System.Address; list : System.Address) return stdio_h.ssize_t;  -- /usr/include/libusb-1.0/libusb.h:1309
   pragma Import (C, libusb_get_device_list, "libusb_get_device_list");

   procedure libusb_free_device_list (list : System.Address; unref_devices : int);  -- /usr/include/libusb-1.0/libusb.h:1311
   pragma Import (C, libusb_free_device_list, "libusb_free_device_list");

   function libusb_ref_device (dev : System.Address) return System.Address;  -- /usr/include/libusb-1.0/libusb.h:1313
   pragma Import (C, libusb_ref_device, "libusb_ref_device");

   procedure libusb_unref_device (dev : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1314
   pragma Import (C, libusb_unref_device, "libusb_unref_device");

   function libusb_get_configuration (dev : System.Address; config : access int) return int;  -- /usr/include/libusb-1.0/libusb.h:1316
   pragma Import (C, libusb_get_configuration, "libusb_get_configuration");

   function libusb_get_device_descriptor (dev : System.Address; desc : access libusb_device_descriptor) return int;  -- /usr/include/libusb-1.0/libusb.h:1318
   pragma Import (C, libusb_get_device_descriptor, "libusb_get_device_descriptor");

   function libusb_get_active_config_descriptor (dev : System.Address; config : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1320
   pragma Import (C, libusb_get_active_config_descriptor, "libusb_get_active_config_descriptor");

   function libusb_get_config_descriptor
     (dev : System.Address;
      config_index : stdint_h.uint8_t;
      config : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1322
   pragma Import (C, libusb_get_config_descriptor, "libusb_get_config_descriptor");

   function libusb_get_config_descriptor_by_value
     (dev : System.Address;
      bConfigurationValue : stdint_h.uint8_t;
      config : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1324
   pragma Import (C, libusb_get_config_descriptor_by_value, "libusb_get_config_descriptor_by_value");

   procedure libusb_free_config_descriptor (config : access libusb_config_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1326
   pragma Import (C, libusb_free_config_descriptor, "libusb_free_config_descriptor");

   function libusb_get_ss_endpoint_companion_descriptor
     (ctx : System.Address;
      endpoint : access constant libusb_endpoint_descriptor;
      ep_comp : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1328
   pragma Import (C, libusb_get_ss_endpoint_companion_descriptor, "libusb_get_ss_endpoint_companion_descriptor");

   procedure libusb_free_ss_endpoint_companion_descriptor (ep_comp : access libusb_ss_endpoint_companion_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1332
   pragma Import (C, libusb_free_ss_endpoint_companion_descriptor, "libusb_free_ss_endpoint_companion_descriptor");

   function libusb_get_bos_descriptor (handle : System.Address; bos : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1334
   pragma Import (C, libusb_get_bos_descriptor, "libusb_get_bos_descriptor");

   procedure libusb_free_bos_descriptor (bos : access libusb_bos_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1336
   pragma Import (C, libusb_free_bos_descriptor, "libusb_free_bos_descriptor");

   function libusb_get_usb_2_0_extension_descriptor
     (ctx : System.Address;
      dev_cap : access libusb_bos_dev_capability_descriptor;
      usb_2_0_extension : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1337
   pragma Import (C, libusb_get_usb_2_0_extension_descriptor, "libusb_get_usb_2_0_extension_descriptor");

   procedure libusb_free_usb_2_0_extension_descriptor (usb_2_0_extension : access libusb_usb_2_0_extension_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1341
   pragma Import (C, libusb_free_usb_2_0_extension_descriptor, "libusb_free_usb_2_0_extension_descriptor");

   function libusb_get_ss_usb_device_capability_descriptor
     (ctx : System.Address;
      dev_cap : access libusb_bos_dev_capability_descriptor;
      ss_usb_device_cap : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1343
   pragma Import (C, libusb_get_ss_usb_device_capability_descriptor, "libusb_get_ss_usb_device_capability_descriptor");

   procedure libusb_free_ss_usb_device_capability_descriptor (ss_usb_device_cap : access libusb_ss_usb_device_capability_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1347
   pragma Import (C, libusb_free_ss_usb_device_capability_descriptor, "libusb_free_ss_usb_device_capability_descriptor");

   function libusb_get_container_id_descriptor
     (ctx : System.Address;
      dev_cap : access libusb_bos_dev_capability_descriptor;
      container_id : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1349
   pragma Import (C, libusb_get_container_id_descriptor, "libusb_get_container_id_descriptor");

   procedure libusb_free_container_id_descriptor (container_id : access libusb_container_id_descriptor);  -- /usr/include/libusb-1.0/libusb.h:1352
   pragma Import (C, libusb_free_container_id_descriptor, "libusb_free_container_id_descriptor");

   function libusb_get_bus_number (dev : System.Address) return stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:1354
   pragma Import (C, libusb_get_bus_number, "libusb_get_bus_number");

   function libusb_get_port_number (dev : System.Address) return stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:1355
   pragma Import (C, libusb_get_port_number, "libusb_get_port_number");

   function libusb_get_port_numbers
     (dev : System.Address;
      port_numbers : access stdint_h.uint8_t;
      port_numbers_len : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1356
   pragma Import (C, libusb_get_port_numbers, "libusb_get_port_numbers");

   function libusb_get_port_path
     (ctx : System.Address;
      dev : System.Address;
      path : access stdint_h.uint8_t;
      path_length : stdint_h.uint8_t) return int;  -- /usr/include/libusb-1.0/libusb.h:1358
   pragma Import (C, libusb_get_port_path, "libusb_get_port_path");

   function libusb_get_parent (dev : System.Address) return System.Address;  -- /usr/include/libusb-1.0/libusb.h:1359
   pragma Import (C, libusb_get_parent, "libusb_get_parent");

   function libusb_get_device_address (dev : System.Address) return stdint_h.uint8_t;  -- /usr/include/libusb-1.0/libusb.h:1360
   pragma Import (C, libusb_get_device_address, "libusb_get_device_address");

   function libusb_get_device_speed (dev : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1361
   pragma Import (C, libusb_get_device_speed, "libusb_get_device_speed");

   function libusb_get_max_packet_size (dev : System.Address; endpoint : unsigned_char) return int;  -- /usr/include/libusb-1.0/libusb.h:1362
   pragma Import (C, libusb_get_max_packet_size, "libusb_get_max_packet_size");

   function libusb_get_max_iso_packet_size (dev : System.Address; endpoint : unsigned_char) return int;  -- /usr/include/libusb-1.0/libusb.h:1364
   pragma Import (C, libusb_get_max_iso_packet_size, "libusb_get_max_iso_packet_size");

   function libusb_open (dev : System.Address; handle : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1367
   pragma Import (C, libusb_open, "libusb_open");

   procedure libusb_close (dev_handle : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1368
   pragma Import (C, libusb_close, "libusb_close");

   function libusb_get_device (dev_handle : System.Address) return System.Address;  -- /usr/include/libusb-1.0/libusb.h:1369
   pragma Import (C, libusb_get_device, "libusb_get_device");

   function libusb_set_configuration (dev : System.Address; configuration : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1371
   pragma Import (C, libusb_set_configuration, "libusb_set_configuration");

   function libusb_claim_interface (dev : System.Address; interface_number : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1373
   pragma Import (C, libusb_claim_interface, "libusb_claim_interface");

   function libusb_release_interface (dev : System.Address; interface_number : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1375
   pragma Import (C, libusb_release_interface, "libusb_release_interface");

   function libusb_open_device_with_vid_pid
     (ctx : System.Address;
      vendor_id : stdint_h.uint16_t;
      product_id : stdint_h.uint16_t) return System.Address;  -- /usr/include/libusb-1.0/libusb.h:1378
   pragma Import (C, libusb_open_device_with_vid_pid, "libusb_open_device_with_vid_pid");

   function libusb_set_interface_alt_setting
     (dev : System.Address;
      interface_number : int;
      alternate_setting : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1381
   pragma Import (C, libusb_set_interface_alt_setting, "libusb_set_interface_alt_setting");

   function libusb_clear_halt (dev : System.Address; endpoint : unsigned_char) return int;  -- /usr/include/libusb-1.0/libusb.h:1383
   pragma Import (C, libusb_clear_halt, "libusb_clear_halt");

   function libusb_reset_device (dev : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1385
   pragma Import (C, libusb_reset_device, "libusb_reset_device");

   function libusb_kernel_driver_active (dev : System.Address; interface_number : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1387
   pragma Import (C, libusb_kernel_driver_active, "libusb_kernel_driver_active");

   function libusb_detach_kernel_driver (dev : System.Address; interface_number : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1389
   pragma Import (C, libusb_detach_kernel_driver, "libusb_detach_kernel_driver");

   function libusb_attach_kernel_driver (dev : System.Address; interface_number : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1391
   pragma Import (C, libusb_attach_kernel_driver, "libusb_attach_kernel_driver");

   function libusb_set_auto_detach_kernel_driver (dev : System.Address; enable : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1393
   pragma Import (C, libusb_set_auto_detach_kernel_driver, "libusb_set_auto_detach_kernel_driver");

   function libusb_control_transfer_get_data (transfer : access libusb_transfer) return access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1410
   pragma Import (C, libusb_control_transfer_get_data, "libusb_control_transfer_get_data");

   function libusb_control_transfer_get_setup (transfer : access libusb_transfer) return access libusb_control_setup;  -- /usr/include/libusb-1.0/libusb.h:1428
   pragma Import (C, libusb_control_transfer_get_setup, "libusb_control_transfer_get_setup");

   procedure libusb_fill_control_setup
     (buffer : access unsigned_char;
      bmRequestType : stdint_h.uint8_t;
      bRequest : stdint_h.uint8_t;
      wValue : stdint_h.uint16_t;
      wIndex : stdint_h.uint16_t;
      wLength : stdint_h.uint16_t);  -- /usr/include/libusb-1.0/libusb.h:1457
   pragma Import (C, libusb_fill_control_setup, "libusb_fill_control_setup");

   function libusb_alloc_transfer (iso_packets : int) return access libusb_transfer;  -- /usr/include/libusb-1.0/libusb.h:1469
   pragma Import (C, libusb_alloc_transfer, "libusb_alloc_transfer");

   function libusb_submit_transfer (transfer : access libusb_transfer) return int;  -- /usr/include/libusb-1.0/libusb.h:1470
   pragma Import (C, libusb_submit_transfer, "libusb_submit_transfer");

   function libusb_cancel_transfer (transfer : access libusb_transfer) return int;  -- /usr/include/libusb-1.0/libusb.h:1471
   pragma Import (C, libusb_cancel_transfer, "libusb_cancel_transfer");

   procedure libusb_free_transfer (transfer : access libusb_transfer);  -- /usr/include/libusb-1.0/libusb.h:1472
   pragma Import (C, libusb_free_transfer, "libusb_free_transfer");

   procedure libusb_fill_control_transfer
     (transfer : access libusb_transfer;
      dev_handle : System.Address;
      buffer : access unsigned_char;
      callback : libusb_transfer_cb_fn;
      user_data : System.Address;
      timeout : unsigned);  -- /usr/include/libusb-1.0/libusb.h:1502
   pragma Import (C, libusb_fill_control_transfer, "libusb_fill_control_transfer");

   procedure libusb_fill_bulk_transfer
     (transfer : access libusb_transfer;
      dev_handle : System.Address;
      endpoint : unsigned_char;
      buffer : access unsigned_char;
      length : int;
      callback : libusb_transfer_cb_fn;
      user_data : System.Address;
      timeout : unsigned);  -- /usr/include/libusb-1.0/libusb.h:1533
   pragma Import (C, libusb_fill_bulk_transfer, "libusb_fill_bulk_transfer");

   procedure libusb_fill_interrupt_transfer
     (transfer : access libusb_transfer;
      dev_handle : System.Address;
      endpoint : unsigned_char;
      buffer : access unsigned_char;
      length : int;
      callback : libusb_transfer_cb_fn;
      user_data : System.Address;
      timeout : unsigned);  -- /usr/include/libusb-1.0/libusb.h:1561
   pragma Import (C, libusb_fill_interrupt_transfer, "libusb_fill_interrupt_transfer");

   procedure libusb_fill_iso_transfer
     (transfer : access libusb_transfer;
      dev_handle : System.Address;
      endpoint : unsigned_char;
      buffer : access unsigned_char;
      length : int;
      num_iso_packets : int;
      callback : libusb_transfer_cb_fn;
      user_data : System.Address;
      timeout : unsigned);  -- /usr/include/libusb-1.0/libusb.h:1590
   pragma Import (C, libusb_fill_iso_transfer, "libusb_fill_iso_transfer");

   procedure libusb_set_iso_packet_lengths (transfer : access libusb_transfer; length : unsigned);  -- /usr/include/libusb-1.0/libusb.h:1614
   pragma Import (C, libusb_set_iso_packet_lengths, "libusb_set_iso_packet_lengths");

   function libusb_get_iso_packet_buffer (transfer : access libusb_transfer; packet : unsigned) return access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1638
   pragma Import (C, libusb_get_iso_packet_buffer, "libusb_get_iso_packet_buffer");

   function libusb_get_iso_packet_buffer_simple (transfer : access libusb_transfer; packet : unsigned) return access unsigned_char;  -- /usr/include/libusb-1.0/libusb.h:1680
   pragma Import (C, libusb_get_iso_packet_buffer_simple, "libusb_get_iso_packet_buffer_simple");

   function libusb_control_transfer
     (dev_handle : System.Address;
      request_type : stdint_h.uint8_t;
      bRequest : stdint_h.uint8_t;
      wValue : stdint_h.uint16_t;
      wIndex : stdint_h.uint16_t;
      data : access unsigned_char;
      wLength : stdint_h.uint16_t;
      timeout : unsigned) return int;  -- /usr/include/libusb-1.0/libusb.h:1700
   pragma Import (C, libusb_control_transfer, "libusb_control_transfer");

   function libusb_bulk_transfer
     (dev_handle : System.Address;
      endpoint : unsigned_char;
      data : access unsigned_char;
      length : int;
      actual_length : access int;
      timeout : unsigned) return int;  -- /usr/include/libusb-1.0/libusb.h:1704
   pragma Import (C, libusb_bulk_transfer, "libusb_bulk_transfer");

   function libusb_interrupt_transfer
     (dev_handle : System.Address;
      endpoint : unsigned_char;
      data : access unsigned_char;
      length : int;
      actual_length : access int;
      timeout : unsigned) return int;  -- /usr/include/libusb-1.0/libusb.h:1708
   pragma Import (C, libusb_interrupt_transfer, "libusb_interrupt_transfer");

   function libusb_get_descriptor
     (dev : System.Address;
      desc_type : stdint_h.uint8_t;
      desc_index : stdint_h.uint8_t;
      data : access unsigned_char;
      length : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1724
   pragma Import (C, libusb_get_descriptor, "libusb_get_descriptor");

   function libusb_get_string_descriptor
     (dev : System.Address;
      desc_index : stdint_h.uint8_t;
      langid : stdint_h.uint16_t;
      data : access unsigned_char;
      length : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1746
   pragma Import (C, libusb_get_string_descriptor, "libusb_get_string_descriptor");

   function libusb_get_string_descriptor_ascii
     (dev : System.Address;
      desc_index : stdint_h.uint8_t;
      data : access unsigned_char;
      length : int) return int;  -- /usr/include/libusb-1.0/libusb.h:1754
   pragma Import (C, libusb_get_string_descriptor_ascii, "libusb_get_string_descriptor_ascii");

   function libusb_try_lock_events (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1759
   pragma Import (C, libusb_try_lock_events, "libusb_try_lock_events");

   procedure libusb_lock_events (ctx : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1760
   pragma Import (C, libusb_lock_events, "libusb_lock_events");

   procedure libusb_unlock_events (ctx : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1761
   pragma Import (C, libusb_unlock_events, "libusb_unlock_events");

   function libusb_event_handling_ok (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1762
   pragma Import (C, libusb_event_handling_ok, "libusb_event_handling_ok");

   function libusb_event_handler_active (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1763
   pragma Import (C, libusb_event_handler_active, "libusb_event_handler_active");

   procedure libusb_lock_event_waiters (ctx : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1764
   pragma Import (C, libusb_lock_event_waiters, "libusb_lock_event_waiters");

   procedure libusb_unlock_event_waiters (ctx : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1765
   pragma Import (C, libusb_unlock_event_waiters, "libusb_unlock_event_waiters");

   function libusb_wait_for_event (ctx : System.Address; tv : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/libusb-1.0/libusb.h:1766
   pragma Import (C, libusb_wait_for_event, "libusb_wait_for_event");

   function libusb_handle_events_timeout (ctx : System.Address; tv : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/libusb-1.0/libusb.h:1768
   pragma Import (C, libusb_handle_events_timeout, "libusb_handle_events_timeout");

   function libusb_handle_events_timeout_completed
     (ctx : System.Address;
      tv : access x86_64_linux_gnu_bits_time_h.timeval;
      completed : access int) return int;  -- /usr/include/libusb-1.0/libusb.h:1770
   pragma Import (C, libusb_handle_events_timeout_completed, "libusb_handle_events_timeout_completed");

   function libusb_handle_events (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1772
   pragma Import (C, libusb_handle_events, "libusb_handle_events");

   function libusb_handle_events_completed (ctx : System.Address; completed : access int) return int;  -- /usr/include/libusb-1.0/libusb.h:1773
   pragma Import (C, libusb_handle_events_completed, "libusb_handle_events_completed");

   function libusb_handle_events_locked (ctx : System.Address; tv : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/libusb-1.0/libusb.h:1774
   pragma Import (C, libusb_handle_events_locked, "libusb_handle_events_locked");

   function libusb_pollfds_handle_timeouts (ctx : System.Address) return int;  -- /usr/include/libusb-1.0/libusb.h:1776
   pragma Import (C, libusb_pollfds_handle_timeouts, "libusb_pollfds_handle_timeouts");

   function libusb_get_next_timeout (ctx : System.Address; tv : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/libusb-1.0/libusb.h:1777
   pragma Import (C, libusb_get_next_timeout, "libusb_get_next_timeout");

   type libusb_pollfd is record
      fd : aliased int;  -- /usr/include/libusb-1.0/libusb.h:1785
      events : aliased short;  -- /usr/include/libusb-1.0/libusb.h:1791
   end record;
   pragma Convention (C_Pass_By_Copy, libusb_pollfd);  -- /usr/include/libusb-1.0/libusb.h:1783

   type libusb_pollfd_added_cb is access procedure
        (arg1 : int;
         arg2 : short;
         arg3 : System.Address);
   pragma Convention (C, libusb_pollfd_added_cb);  -- /usr/include/libusb-1.0/libusb.h:1804

   type libusb_pollfd_removed_cb is access procedure (arg1 : int; arg2 : System.Address);
   pragma Convention (C, libusb_pollfd_removed_cb);  -- /usr/include/libusb-1.0/libusb.h:1816

   function libusb_get_pollfds (ctx : System.Address) return System.Address;  -- /usr/include/libusb-1.0/libusb.h:1818
   pragma Import (C, libusb_get_pollfds, "libusb_get_pollfds");

   procedure libusb_set_pollfd_notifiers
     (ctx : System.Address;
      added_cb : libusb_pollfd_added_cb;
      removed_cb : libusb_pollfd_removed_cb;
      user_data : System.Address);  -- /usr/include/libusb-1.0/libusb.h:1820
   pragma Import (C, libusb_set_pollfd_notifiers, "libusb_set_pollfd_notifiers");

   subtype libusb_hotplug_callback_handle is int;  -- /usr/include/libusb-1.0/libusb.h:1836

   subtype libusb_hotplug_flag is unsigned;
   LIBUSB_HOTPLUG_ENUMERATE : constant libusb_hotplug_flag := 1;  -- /usr/include/libusb-1.0/libusb.h:1846

   subtype libusb_hotplug_event is unsigned;
   LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED : constant libusb_hotplug_event := 1;
   LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT : constant libusb_hotplug_event := 2;  -- /usr/include/libusb-1.0/libusb.h:1861

   type libusb_hotplug_callback_fn is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : libusb_hotplug_event;
         arg4 : System.Address) return int;
   pragma Convention (C, libusb_hotplug_callback_fn);  -- /usr/include/libusb-1.0/libusb.h:1889

   function libusb_hotplug_register_callback
     (ctx : System.Address;
      events : libusb_hotplug_event;
      flags : libusb_hotplug_flag;
      vendor_id : int;
      product_id : int;
      dev_class : int;
      cb_fn : libusb_hotplug_callback_fn;
      user_data : System.Address;
      handle : access libusb_hotplug_callback_handle) return int;  -- /usr/include/libusb-1.0/libusb.h:1928
   pragma Import (C, libusb_hotplug_register_callback, "libusb_hotplug_register_callback");

   procedure libusb_hotplug_deregister_callback (ctx : System.Address; handle : libusb_hotplug_callback_handle);  -- /usr/include/libusb-1.0/libusb.h:1948
   pragma Import (C, libusb_hotplug_deregister_callback, "libusb_hotplug_deregister_callback");

end libusb_1_0_libusb_h;
