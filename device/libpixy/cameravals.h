#ifndef CAMERAVALS_H
#define CAMERAVALS_H

#define CAM_MODE0               0x00
#define CAM_MODE1               0x01

#define CAM_LIGHT_NORMAL        0
#define CAM_LIGHT_LOW           1
#define CAM_LIGHT_HIGH          2 // not sure if combining high light and low light exposure is possible, or good

#define CAM_BRIGHTNESS_DEFAULT  70
#define CAM_BRIGHTNESS_RANGE    0x14


#define CAM_RES0                0x00 
#define CAM_RES1                0x01
#define CAM_RES2                0x02
#define CAM_RES0_WIDTH          1280
#define CAM_RES0_HEIGHT         800
#define CAM_RES1_WIDTH          640
#define CAM_RES1_HEIGHT         400
#define CAM_RES2_WIDTH          320
#define CAM_RES2_HEIGHT         200

#define CAM_GRAB_M0R0           (CAM_RES0<<4 | CAM_MODE0)
#define CAM_GRAB_M1R1           (CAM_RES1<<4 | CAM_MODE1)
#define CAM_GRAB_M1R2           (CAM_RES2<<4 | CAM_MODE1)

#endif
