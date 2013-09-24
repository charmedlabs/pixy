#ifndef _SPI_H
#define _SPI_H

#define SPI_RECEIVEBUF_SIZE   1
#define SPI_TRANSMITBUF_SIZE  16
#define RECEIVE_LEN()     (g_receive.m_produced - g_receive.m_consumed) 	// note, don't need to deal with overflow because of the way ints subtract

typedef uint32_t (*TransmitCallback)(uint16_t *data, uint32_t len); 	

int spi_checkIdle();
int spi_sync();
int spi_receive(uint16_t *buf, uint32_t len);
void spi_setCallback(TransmitCallback callback);
void spi_init();

#endif
