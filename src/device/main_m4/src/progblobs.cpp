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

#include "blobs.h"
#include "progblobs.h"
#include "pixy_init.h"
#include "camera.h"
#include "led.h"
#include "serial.h"
#include "exec.h"


static int blobsSetup();
static int blobsLoop();

Program g_progBlobs =
{
    "Color_connected_components",
    "perform color connected components",
    blobsSetup,
    blobsLoop
};

static bool initialized_ = false;
static Qqueue qqueue_;
static Blobs blobs_;

static uint32_t callback(uint8_t *data, uint32_t len)
{
    return blobs_.getBlock(data, len);
}

static int sendBlobs(Chirp *chirp, const BlobA *blobs, uint32_t len, uint8_t renderFlags=RENDER_FLAG_FLUSH)
{
    CRP_RETURN(chirp, HTYPE(FOURCC('C','C','B','1')), HINT8(renderFlags), HINT16(CAM_RES2_WIDTH), HINT16(CAM_RES2_HEIGHT), UINTS16(len*sizeof(BlobA)/sizeof(uint16_t), blobs), END);
    return 0;
}

static int blobsSetup()
{
    if (initialized_ == false)
    {
        ser_init(callback);
        initialized_ = true;
    }

    // setup camera mode
    cam_setMode(CAM_MODE1);

    // setup qqueue and M0
    qqueue_.flush();
    exec_runM0(0);

    // flush serial receive queue
    uint8_t c;
    while(ser_getSerial()->receive(&c, 1));

    return 0;
}

static int blobsLoop()
{
    BlobA *blobs;
    uint32_t numBlobs;

    // create blobs
    if (blobs_.blobify(&qqueue_) < 0)
    {
        return 0;
    }

    // send blobs
    blobs_.getBlobs(&blobs, &numBlobs);
    sendBlobs(g_chirpUsb, blobs, numBlobs);

    // can do work here while waiting for more data in queue
    Iserial *serial = ser_getSerial();
    serial->update();
    while(!qqueue_.queued())
    {
        // Just throw away serial input for now.
        uint8_t c;
        serial->receive(&c, 1);
    }

    return 0;
}
