#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include "pixy.h"


// frame should be at least 64000 bytes
int pixy_cam_get_frame(uint8_t mode, uint16_t xoffset, uint16_t yoffset, uint16_t width, uint16_t height, uint8_t *frame)
{
    if (frame == NULL)
    {
        return -1;
    }

    uint8_t *out_pixels;
    int32_t out_fourcc;
    int8_t out_flags;
    uint16_t out_width;
    uint16_t out_height;
    uint32_t out_pixel_cnt;
    int32_t out_response = 0;
    int ret;

    ret = pixy_command("cam_getFrame",
                        CRP_UINT8,  mode,
                        CRP_UINT16, xoffset,
                        CRP_UINT16, yoffset,
                        CRP_UINT16, width,
                        CRP_UINT16, height,
                        END_OUT_ARGS,        // separator
                        &out_response,       // pointer to mem address for return value
                        &out_fourcc,         // contrary to docs, the next 5 args are needed
                        &out_flags,
                        &out_width,
                        &out_height,
                        &out_pixel_cnt,
                        &out_pixels,         // pointer to mem address for returned frame
                        END_IN_ARGS);

    if (ret == 0)
    {
        memcpy(frame, out_pixels, out_pixel_cnt);
        //printf("returned w %d h %d npix %d\n", out_width, out_height, out_pixel_cnt);
    }

    //printf("getFrame returned %d response %d\n", ret, out_response);
    return 0;
}
