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


#ifndef ROBOMODULE_H
#define ROBOMODULE_H

#include "monmodule.h"

#include "monparameterdb.h"
#include "dataexport.h"

#include <fstream>


inline void ASSERT(bool v)
{
	if (!v)
		abort();
}

template <typename T>
class RawFrame;

template <typename T>
class RawFrameAccess
{
public:
	explicit RawFrameAccess(uint16_t width, uint16_t height, T* frame)
		: width(width)
		, height(height)
		, frame(frame)
	{
	}

	uint16_t getWidth() const
	{
		return width;
	}

	uint16_t getHeight() const
	{
		return height;
	}

	uint32_t getFrameLen() const
	{
		return width*height;
	}

	T* getData()
	{
		return frame;
	}

	T& px(uint16_t x, uint16_t y)
	{
		return *(frame + y*width + x);
	}

	template <typename Fn>
	void forEach(Fn fn)
	{
		for (uint16_t y = 0; y < height; ++y)
		{
			for (uint16_t x = 0; x < width; ++x)
			{
				fn(x, y);
			}
		}
	}

	template <typename TResult, typename FnFilterPixel>
	void applyFilter(TResult& result, FnFilterPixel fn)
	{
		ASSERT(result.getWidth() == getWidth());
		ASSERT(result.getHeight() == getHeight());
		forEach([&](uint16_t x, uint16_t y)
		{
			result.px(x, y) = fn(x, y);
		});
	}

	template <typename FnFilterPixel>
	void applyFilter(FnFilterPixel fn);


	void writeToFile();

protected:
	uint16_t width;
	uint16_t height;
	T* frame;
};

template <typename T>
struct RawFrameStorage
{
	explicit RawFrameStorage(size_t size)
		: frameData(size)
	{
	}

	T* data()
	{
		return frameData.data();
	}

	std::vector<T> frameData;
};

template <>
struct RawFrameStorage<bool>
{
	explicit RawFrameStorage(size_t size)
		: frameData(size)
	{
	}

	bool* data()
	{
		return reinterpret_cast<bool*>(frameData.data());
	}

	std::vector<uint8_t> frameData;
};

template <typename T>
class RawFrame : private RawFrameStorage<T>, public RawFrameAccess<T>
{
public:
	RawFrame(uint16_t width, uint16_t height)
		: RawFrameStorage<T>(width * height)
		, RawFrameAccess<T>(width, height, this->data())
	{
	}

	static RawFrame<T> readFromFile(uint32_t imgNo)
	{
		std::ifstream strm;
		strm.open((MonParameterDB::docPath() + "/image" + QString::number(imgNo) + ".raw").toStdString(), std::ios::in | std::ios::binary);
		uint16_t width, height, frameLen;
		strm.read(reinterpret_cast<char*>(&width), sizeof(uint16_t));
		strm.read(reinterpret_cast<char*>(&height), sizeof(uint16_t));
		strm.read(reinterpret_cast<char*>(&frameLen), sizeof(uint32_t));

		if (height == 0)
		{
			height = 200; //bug circumvention
		}

		ASSERT(frameLen == width*height);
		RawFrame<T> frame(width, height);

		strm.read(reinterpret_cast<char*>(frame.frameData.data()), frameLen);

		return frame;
	}
};

template <typename T>
template <typename FnFilterPixel>
void RawFrameAccess<T>::applyFilter(FnFilterPixel fn)
{
	RawFrame<uint8_t> tmp(width, height);
	applyFilter(tmp, fn);
	std::function<uint8_t&(uint16_t, uint16_t)> fnPx = std::bind(&RawFrameAccess<T>::px, this, std::placeholders::_1, std::placeholders::_2);
	forEach([&, fnPx](uint16_t x, uint16_t y)
	{
		fnPx(x, y) = tmp.px(x, y);
	});
}

template <typename T>
void RawFrameAccess<T>::writeToFile()
{
	{
		QString filename = uniqueFilename(MonParameterDB::docPath(), "image", "raw");
		std::ofstream strm;
		strm.open(filename.toStdString(), std::ios::out | std::ios::binary);
		strm.write(reinterpret_cast<char*>(&width), sizeof(uint16_t));
		strm.write(reinterpret_cast<char*>(&height), sizeof(uint16_t));
		uint32_t frameLen = width*height;
		strm.write(reinterpret_cast<char*>(&frameLen), sizeof(uint32_t));
		strm.write(reinterpret_cast<char*>(frame), frameLen);
	}
}

struct RoboParameters
{
	uint16_t limit = 120;
	uint16_t xLimit = 30;
};

class RoboModule : public MonModule
{
public:
	explicit RoboModule(Interpreter *interpreter);

	virtual bool render(uint32_t fourcc, const void *args[]);
	virtual bool command(const QStringList &argv);
	virtual void paramChange();

	static void debugRender(std::function<Renderer*()> makeRenderer, Renderer* pRenderer, RawFrameAccess<uint8_t>& rawFrame, RoboParameters params = RoboParameters());
};

#endif //ROBOMODULE_H
