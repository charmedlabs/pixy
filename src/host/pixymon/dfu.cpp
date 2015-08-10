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

#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <QThread>
#include "dfu.h"
#include "pixydefs.h"
#include "sleeper.h"


#define milli_sleep(msec) Sleeper::msleep(msec)

/* Find DFU interfaces in a given device.
 * Iterate through all DFU interfaces and their alternate settings
 * and call the passed handler function on each setting until handler
 * returns non-zero. */
static int find_dfu_if(libusb_device *dev,
                       int (*handler)(struct dfu_if *, void *),
                       void *v)
{
    struct libusb_device_descriptor desc;
    struct libusb_config_descriptor *cfg;
    const struct libusb_interface_descriptor *intf;
    const struct libusb_interface *uif;
    struct dfu_if _dif, *dfu_if = &_dif;
    int cfg_idx, intf_idx, alt_idx;
    int rc;

    memset(dfu_if, 0, sizeof(*dfu_if));
    rc = libusb_get_device_descriptor(dev, &desc);
    if (rc)
        return rc;
    for (cfg_idx = 0; cfg_idx < desc.bNumConfigurations;
         cfg_idx++) {
        rc = libusb_get_config_descriptor(dev, cfg_idx, &cfg);
        if (rc)
            return rc;
        /* in some cases, noticably FreeBSD if uid != 0,
                 * the configuration descriptors are empty */
        if (!cfg)
            return 0;
        for (intf_idx = 0; intf_idx < cfg->bNumInterfaces;
             intf_idx++) {
            uif = &cfg->interface[intf_idx];
            if (!uif)
                return 0;
            for (alt_idx = 0;
                 alt_idx < uif->num_altsetting; alt_idx++) {
                intf = &uif->altsetting[alt_idx];
                if (!intf)
                    return 0;
                if (intf->bInterfaceClass == 0xfe &&
                        intf->bInterfaceSubClass == 1) {
                    dfu_if->dev = dev;
                    dfu_if->vendor = desc.idVendor;
                    dfu_if->product = desc.idProduct;
                    dfu_if->bcdDevice = desc.bcdDevice;
                    dfu_if->configuration = cfg->
                            bConfigurationValue;
                    dfu_if->interface =
                            intf->bInterfaceNumber;
                    dfu_if->altsetting =
                            intf->bAlternateSetting;
                    if (intf->bInterfaceProtocol == 2)
                        dfu_if->flags |= DFU_IFF_DFU;
                    else
                        dfu_if->flags &= ~DFU_IFF_DFU;
                    if (!handler)
                        return 1;
                    rc = handler(dfu_if, v);
                    if (rc != 0)
                        return rc;
                }
            }
        }

        libusb_free_config_descriptor(cfg);
    }

    return 0;
}


static int _count_cb(struct dfu_if *dif, void *v)
{
    int *count = (int*) v;

    (*count)++;

    return 0;
}

/* Count DFU interfaces within a single device */
static int count_dfu_interfaces(libusb_device *dev)
{
    int num_found = 0;

    find_dfu_if(dev, &_count_cb, (void *) &num_found);

    return num_found;
}


/* Iterate over all matching DFU capable devices within system */
static int iterate_dfu_devices(libusb_context *ctx, struct dfu_if *dif,
                               int (*action)(struct libusb_device *dev, void *user), void *user)
{
    libusb_device **list;
    ssize_t num_devs, i;

    num_devs = libusb_get_device_list(ctx, &list);
    for (i = 0; i < num_devs; ++i) {
        int retval;
        struct libusb_device_descriptor desc;
        struct libusb_device *dev = list[i];

        if (dif && (dif->flags & DFU_IFF_DEVNUM) &&
                (libusb_get_bus_number(dev) != dif->bus ||
                 libusb_get_device_address(dev) != dif->devnum))
            continue;
        if (libusb_get_device_descriptor(dev, &desc))
            continue;
        if (dif && (dif->flags & DFU_IFF_VENDOR) &&
                desc.idVendor != dif->vendor)
            continue;
        if (dif && (dif->flags & DFU_IFF_PRODUCT) &&
                desc.idProduct != dif->product)
            continue;
        if (!count_dfu_interfaces(dev))
            continue;

        retval = action(dev, user);
        if (retval) {
            libusb_free_device_list(list, 0);
            return retval;
        }
    }
    libusb_free_device_list(list, 0);
    return 0;
}


static int found_dfu_device(struct libusb_device *dev, void *user)
{
    struct dfu_if *dif = (struct dfu_if*) user;

    dif->dev = dev;
    return 1;
}


/* Find the first DFU-capable device, save it in dfu_if->dev */
static int get_first_dfu_device(libusb_context *ctx, struct dfu_if *dif)
{
    return iterate_dfu_devices(ctx, dif, found_dfu_device, dif);
}


static int count_one_dfu_device(struct libusb_device *dev, void *user)
{
    int *num = (int*) user;

    (*num)++;
    return 0;
}


/* Count DFU capable devices within system */
static int count_dfu_devices(libusb_context *ctx, struct dfu_if *dif)
{
    int num_found = 0;

    iterate_dfu_devices(ctx, dif, count_one_dfu_device, &num_found);
    return num_found;
}


/* Look for a descriptor in a concatenated descriptor list
 * Will return desc_index'th match of given descriptor type
 * Returns length of found descriptor, limited to res_size */
static int find_descriptor(const unsigned char *desc_list, int list_len,
                           uint8_t desc_type, uint8_t desc_index,
                           uint8_t *res_buf, int res_size)
{
    int p = 0;
    int hit = 0;

    while (p + 1 < list_len) {
        int desclen;

        desclen = (int) desc_list[p];
        if (desclen == 0) {
            return -1;
        }
        if (desc_list[p + 1] == desc_type && hit++ == desc_index) {
            if (desclen > res_size)
                desclen = res_size;
            if (p + desclen > list_len)
                desclen = list_len - p;
            memcpy(res_buf, &desc_list[p], desclen);
            return desclen;
        }
        p += (int) desc_list[p];
    }
    return 0;
}

/* Look for a descriptor in the active configuration
 * Will also find extra descriptors which are normally
 * not returned by the standard libusb_get_descriptor() */
static int usb_get_any_descriptor(struct libusb_device_handle *dev_handle,
                                  uint8_t desc_type,
                                  uint8_t desc_index,
                                  unsigned char *resbuf, int res_len)
{
    struct libusb_device *dev;
    struct libusb_config_descriptor *config;
    int ret;
    uint16_t conflen;
    unsigned char *cbuf;

    dev = libusb_get_device(dev_handle);
    if (!dev) {
        return -1;
    }
    /* Get the total length of the configuration descriptors */
    ret = libusb_get_active_config_descriptor(dev, &config);
    if (ret)
        return -1;
    conflen = config->wTotalLength;
    libusb_free_config_descriptor(config);

    /* Suck in the configuration descriptor list from device */
    cbuf = (unsigned char *)malloc(conflen);
    ret = libusb_get_descriptor(dev_handle, LIBUSB_DT_CONFIG,
                                desc_index, cbuf, conflen);
    if (ret < conflen) {
        conflen = ret;
    }
    /* Search through the configuration descriptor list */
    ret = find_descriptor(cbuf, conflen, desc_type, desc_index,
                          resbuf, res_len);
    free(cbuf);

    /* A descriptor must be at least 2 bytes long */
    if (ret > 1) {
        return ret;
    }

    /* Finally try to retrieve it requesting the device directly
         * This is not supported on all devices for non-standard types */
    return libusb_get_descriptor(dev_handle, desc_type, desc_index,
                                 resbuf, res_len);
}

Dfu::Dfu()
{
    usb_dfu_func_descriptor func_dfu = {0};
    struct libusb_device_descriptor desc;
    int ret;

    m_context = 0;
    m_transfer_size = 0;
    memset(&m_dif, 0, sizeof(m_dif));
    ret = libusb_init(&m_context);
    if (ret)
        throw std::runtime_error("Cannot initialize USB.");

#if 0
    num_devs = count_dfu_devices(m_context, &m_dif);
    if (num_devs != 1)
        throw std::runtime_error("Cannot find Pixy DFU device.");
    if (!get_first_dfu_device(m_context, &m_dif))
        throw std::runtime_error("Cannot find Pixy DFU device.");
    ret = libusb_open(m_dif.dev, &m_dif.dev_handle);
    if (ret || !m_dif.dev_handle)
        throw std::runtime_error("Cannot open Pixy DFU device.");
#else
    m_dif.dev_handle = libusb_open_device_with_vid_pid(m_context, PIXY_DFU_VID, PIXY_DFU_PID);
    if (m_dif.dev_handle==NULL)
        throw std::runtime_error("Cannot open Pixy DFU device.");
    m_dif.dev = libusb_get_device(m_dif.dev_handle);
#endif
    if (libusb_set_configuration(m_dif.dev_handle, 1) < 0)
        throw std::runtime_error("Cannot set configuration for Pixy DFU device.");
    if (libusb_claim_interface(m_dif.dev_handle, m_dif.interface) < 0)
        throw std::runtime_error("Cannot claim interface for Pixy DFU device.");
    if (libusb_set_interface_alt_setting(m_dif.dev_handle, m_dif.interface, m_dif.altsetting) < 0)
        throw std::runtime_error("Cannot set interface for Pixy DFU device.");

    ret = usb_get_any_descriptor(m_dif.dev_handle,
                                 USB_DT_DFU, 0,
                                 (unsigned char *) &func_dfu,
                                 sizeof(func_dfu));
    m_transfer_size = libusb_le16_to_cpu(func_dfu.wTransferSize);
    if (!m_transfer_size)
        throw std::runtime_error("Cannot get transfer size for Pixy DFU device.");

    /* DFU specification */
    if (libusb_get_device_descriptor(m_dif.dev, &desc))
        throw std::runtime_error("Cannot get descriptor for Pixy DFU device.");
    if (m_transfer_size < desc.bMaxPacketSize0)
        m_transfer_size = desc.bMaxPacketSize0;
}

Dfu::~Dfu()
{
    if (m_dif.dev_handle)
        libusb_close(m_dif.dev_handle);
    if (m_context)
        libusb_exit(m_context);
}


/*
 *  DFU_DNLOAD Request (DFU Spec 1.0, Section 6.1.1)
 *
 *  device    - the usb_dev_handle to communicate with
 *  interface - the interface to communicate with
 *  length    - the total number of bytes to transfer to the USB
 *              device - must be less than wTransferSize
 *  data      - the data to transfer
 *
 *  returns the number of bytes written or < 0 on error
 */
int dfu_download( libusb_device_handle *device,
                  const unsigned short interface,
                  const unsigned short length,
                  unsigned char* data )
{
    int status;

    /* Sanity checks */
    if( (0 != length) && (NULL == data) ) {
        return -1;
    }

    if( (0 == length) && (NULL != data) ) {
        return -2;
    }

    status = libusb_control_transfer( device,
                                      /* bmRequestType */ LIBUSB_ENDPOINT_OUT | LIBUSB_REQUEST_TYPE_CLASS | LIBUSB_RECIPIENT_INTERFACE,
                                      /* bRequest      */ DFU_DNLOAD,
                                      /* wValue        */ 0,
                                      /* wIndex        */ interface,
                                      /* Data          */ data,
                                      /* wLength       */ length,
                                      DFU_TIMEOUT );

    return status;
}
/*
 *  DFU_GETSTATUS Request (DFU Spec 1.0, Section 6.1.2)
 *
 *  device    - the usb_dev_handle to communicate with
 *  interface - the interface to communicate with
 *  status    - the data structure to be populated with the results
 *
 *  return the number of bytes read in or < 0 on an error
 */
int dfu_get_status( libusb_device_handle *device,
                    const unsigned short interface,
                    struct dfu_status *status )
{
    unsigned char buffer[6];
    int result;

    /* Initialize the status data structure */
    status->bStatus       = DFU_STATUS_ERROR_UNKNOWN;
    status->bwPollTimeout = 0;
    status->bState        = STATE_DFU_ERROR;
    status->iString       = 0;

    //int len;
    //result = libusb_bulk_transfer( device, LIBUSB_ENDPOINT_IN, buffer, 6, &len, 500);

    result = libusb_control_transfer( device,
                                      /* bmRequestType */ LIBUSB_ENDPOINT_IN | LIBUSB_REQUEST_TYPE_CLASS  | LIBUSB_RECIPIENT_INTERFACE,
                                      /* bRequest      */ DFU_GETSTATUS,
                                      /* wValue        */ 0,
                                      /* wIndex        */ interface,
                                      /* Data          */ buffer,
                                      /* wLength       */ 6,
                                      DFU_TIMEOUT );
    if( 6 == result ) {
        status->bStatus = buffer[0];
        status->bwPollTimeout = ((0xff & buffer[3]) << 16) |
                ((0xff & buffer[2]) << 8)  |
                (0xff & buffer[1]);

        status->bState  = buffer[4];
        status->iString = buffer[5];
    }

    return result;
}

int Dfu::download(const QString &filename)
{
    int bytes_sent = 0;
    unsigned char *buf;
    struct dfu_status dst;
    int ret;
    int fsize;
    unsigned int bytes_left;
    int chunk_size;
    const char *exception = NULL;

    FILE *filep = fopen(filename.toUtf8(), "rb");
    if (filep == NULL)
        throw std::runtime_error("Cannot open file.");

    buf = (unsigned char *)malloc(m_transfer_size);
    fseek(filep, 0, SEEK_END);
    fsize = ftell(filep);
    rewind(filep);

    while (bytes_sent < fsize)
    {
        bytes_left = fsize - bytes_sent;
        if (bytes_left < m_transfer_size)
            chunk_size = bytes_left;
        else
            chunk_size = m_transfer_size;
        ret = fread(buf, 1, chunk_size, filep);
        if (ret < 0)
        {
            exception = "Error while reading file.";
            goto exit;
        }
        ret = dfu_download(m_dif.dev_handle, m_dif.interface, ret, ret ? buf : NULL);
        if (ret < 0)
        {
            exception = "Error while downloading DFU file.";
            goto exit;
        }
        bytes_sent += ret;

        do {
            ret = dfu_get_status(m_dif.dev_handle, m_dif.interface, &dst);
            if (ret < 0)
            {
                exception = "Error while getting status.";
                goto exit;
            }

            if (dst.bState == DFU_STATE_dfuDNLOAD_IDLE ||
                    dst.bState == DFU_STATE_dfuERROR)
                break;

            milli_sleep(dst.bwPollTimeout);

        } while (1);

        if (dst.bStatus != DFU_STATUS_OK)
        {
            exception = "Bad status during DFU download.";
            goto exit;
        }
    }

    /* send one zero sized download request to signalize end */
    ret = dfu_download(m_dif.dev_handle, m_dif.interface, 0, NULL);
    if (ret < 0)
    {
        exception = "Error while downloading DFU file.";
        goto exit;
    }

    /* Transition to MANIFEST_SYNC state */
    ret = dfu_get_status(m_dif.dev_handle, m_dif.interface, &dst);

exit:
    free(buf);
    fclose(filep);
    if (exception)
        throw std::runtime_error(exception);

    return bytes_sent;
}
