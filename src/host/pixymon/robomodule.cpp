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

#include <stdio.h>
#include "robomodule.h"
#include "interpreter.h"
#include "renderer.h"
#include "robo.h"
#include "qqueue.h"
#include <QPainter>

#include <array>
#include <tuple>
#include <algorithm>

//tmp
#include <iostream>
//tmp

// declare module
MON_MODULE(RoboModule);


class FrameAccess
{
public:
	explicit FrameAccess(uint16_t width, uint16_t height, uint32_t frameLen, uint8_t *frame)
		: width(width)
		, height(height)
		, frameLen(frameLen)
		, frame(frame)
	{
	}

	uint8_t& px(uint16_t x, uint16_t y)
	{
		return px(frame, x, y);
	}

	template <typename T>
	T& px(T* frame, uint16_t x, uint16_t y)
	{
		return *(frame + y*width + x);
	}

	uint8_t r(uint16_t x, uint16_t y)
	{
		auto pixel = &px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				return *pixel;
			}
			else
			{
				return (*(pixel-1)+*(pixel+1))>>1;
			}
		}
		else
		{
			if (x&1)
			{
				return (*(pixel-width)+*(pixel+width))>>1;
			}
			else
			{
				return (*(pixel-width-1)+*(pixel-width+1)+*(pixel+width-1)+*(pixel+width+1))>>2;
			}
		}
	}

	uint8_t g(uint16_t x, uint16_t y)
	{
		auto pixel = &px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				return (*(pixel-1)+*(pixel+1)+*(pixel+width)+*(pixel-width))>>2;
			}
			else
			{
				return *pixel;
			}
		}
		else
		{
			if (x&1)
			{
				return *pixel;
			}
			else
			{
				return (*(pixel-1)+*(pixel+1)+*(pixel+width)+*(pixel-width))>>2;
			}
		}
	}

	uint8_t b(uint16_t x, uint16_t y)
	{
		auto pixel = &px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				return (*(pixel-width-1)+*(pixel-width+1)+*(pixel+width-1)+*(pixel+width+1))>>2;
			}
			else
			{
				return (*(pixel-width)+*(pixel+width))>>1;
			}
		}
		else
		{
			if (x&1)
			{
				return (*(pixel-1)+*(pixel+1))>>1;
			}
			else
			{
				return *pixel;
			}
		}
	}

	uint8_t lumina(uint16_t x, uint16_t y)
	{
		return 0.2126 * r(x, y) + 0.7152 * g(x, y) + 0.0722 * b(x, y);
	}

	template <typename Fn>
	void forEachPixel(Fn fn)
	{
		for (uint16_t y = 0; y < height; ++y)
		{
			for (uint16_t x = 0; x < width; ++x)
			{
				fn(x, y);
			}
		}
	}

	void convertToGrayscale()
	{
		forEachPixel([&](uint16_t x, uint16_t y)
		{
			px(x, y) = lumina(x, y);
		});
	}

	void histEqu()
	{
		std::array<uint32_t, std::numeric_limits<uint8_t>::max()> histData;
		for (auto&& entry : histData)
			entry = 0;

		forEachPixel([&](uint16_t x, uint16_t y)
		{
			++histData[px(x, y)];
		});

		uint32_t sumHist = width * height;

		std::array<uint32_t, std::numeric_limits<uint8_t>::max()> cumHistData;
		cumHistData[0] = histData[0];
		for (size_t i = 1; i < std::numeric_limits<uint8_t>::max(); ++i)
		{
			cumHistData[i] = cumHistData[i - 1] + histData[i];
		}

		forEachPixel([&](uint16_t x, uint16_t y)
		{
			px(x, y) = (cumHistData[px(x, y)] * std::numeric_limits<uint8_t>::max()) / sumHist;
		});
	}

	void nonPlateFilter()
	{
		constexpr auto nonPlatePercent = 0.8;
		constexpr auto nonSafetyFactor = 1;

		const auto accumulatedPixels = ((height * (1 - nonPlatePercent)) * width);

		std::vector<uint8_t> relevantPixels;
		relevantPixels.reserve(accumulatedPixels);

		for (uint16_t y = height * nonPlatePercent; y < height; ++y)
		{
			for (uint16_t x = 0; x < width; ++x)
			{
				relevantPixels.push_back(px(x, y));
			}
		}
		std::sort(relevantPixels.begin(), relevantPixels.end());
		auto medianBrightness = relevantPixels[relevantPixels.size() / 2];

		uint8_t brightnessLimit = medianBrightness / nonSafetyFactor;

		applyFilter<uint8_t>([&](uint16_t x, uint16_t y)
		{
			auto val = px(x, y);
			return val > brightnessLimit ? 255 : val;
		});
	}

	void gaussianFilter()
	{
		applyFilter<uint8_t>([&](uint16_t x, uint16_t y)
		{
			return (
						px(x-1, y-1) / 16. +
						px(x,   y-1) / 8. +
						px(x+1, y-1) / 16. +

						px(x-1, y) / 8. +
						px(x,   y) / 4. +
						px(x+1, y) / 8. +

						px(x-1, y+1) / 16. +
						px(x,   y+1) / 8. +
						px(x+2, y+1) / 16.

						);
		});
	}

	void medianFilter()
	{
		applyFilter<uint8_t>([&](uint16_t x, uint16_t y)
		{
			std::vector<uint8_t> values =
			{
				px(x-1, y-1),
				px(x,   y-1),
				px(x+1, y-1),

				px(x-1, y),
				px(x,   y),
				px(x+1, y),

				px(x-1, y+1),
				px(x,   y+1),
				px(x+2, y+1)
			};
			std::sort(values.begin(), values.end());
			return values[values.size() / 2];
		});
	}

	void sobelFilterDlDx()
	{
		sobelFiltered.resize(frameLen);
		applyFilter<int16_t>([&](uint16_t x, uint16_t y)
		{
			return (
						-1 * px(x-1, y-1) +
						-2 * px(x,   y-1) +
						-1 * px(x+1, y-1) +

						1 * px(x-1, y+1) +
						2 * px(x,   y+1) +
						1 * px(x+2, y+1)

						);
		});
	}

	void sobelFilterDlDy()
	{
		sobelFiltered.resize(frameLen);
		applyFilter<int16_t>(sobelFiltered, [&](uint16_t x, uint16_t y)
		{
			return (
						1 * px(x-1, y-1) +
						0 * px(x,   y-1) +
						-1 * px(x+1, y-1) +

						2 * px(x-1, y) +
						0 * px(x,   y) +
						-2 * px(x+1, y) +

						1 * px(x-1, y+1) +
						0 * px(x,   y+1) +
						-1 * px(x+2, y+1)

						);
		});
	}

	void laplaceFilter()
	{
		applyFilter<int16_t>([&](uint16_t x, uint16_t y)
		{
			return (
						0 * px(x-1, y-1) +
						-1 * px(x,   y-1) +
						0 * px(x+1, y-1) +

						-1 * px(x-1, y) +
						4 * px(x,   y) +
						-1 * px(x+1, y) +

						0 * px(x-1, y+1) +
						-1 * px(x,   y+1) +
						0 * px(x+2, y+1)
						);
		});
	}

	void findRoboAfterSobel()
	{
		constexpr auto minDistFromTopPercent = 0.25;
		constexpr auto columnWidth = 1;
		constexpr auto maxDeviationToBeInLine = 0.8;
		constexpr auto minRiseOrFall = 10;
		constexpr auto percentToStripFromEachSide = 0.1;

		const auto columns = width / columnWidth;

		std::vector<std::vector<int16_t>> colValues;
		colValues.resize(columns);

		if (sobelFiltered.empty())
			abort();

		forEachPixel([&](uint16_t x, uint16_t y)
		{
			if (x >= 5 && x < (width-5) &&  //minus 5 pixels left and right (various filters)
				x >= width * percentToStripFromEachSide &&
				x <= width * (1 - percentToStripFromEachSide))
			{
				if (colValues[x / columnWidth].empty())
				{
					colValues[x / columnWidth].resize(height);
				}
				colValues[x / columnWidth][y] += px(sobelFiltered.data(), x, y);
			}
		});

		struct Line
		{
			uint16_t length;
			int32_t averageGradient;
		};

		auto compareLineLength = [](Line lho, Line rho)
		{
			return (std::abs(lho.averageGradient) * lho.length) < (std::abs(rho.averageGradient) * rho.length);
		};

		std::vector<Line> colBestLines;
		colBestLines.resize(columns);
		for (auto i = 0; i < columns; ++i)
		{
			std::vector<Line> colLines;
			std::vector<int32_t> currLineValues;
			for (auto y = height * minDistFromTopPercent; y < colValues[i].size(); ++y)
			{
				const auto& value = colValues[i][y];
				auto isRelevantRiseOrFall = (value > minRiseOrFall || value < -minRiseOrFall);
				if (isRelevantRiseOrFall && (
						currLineValues.empty() ||
						(
							((value >= 0) == (currLineValues.back() >= 0)) && //same sign
							(
								(value > currLineValues.back() * maxDeviationToBeInLine) ||
								(value < currLineValues.back() * (1-maxDeviationToBeInLine))))))
				{
					currLineValues.push_back(value);
				}
				else
				{
					if (!currLineValues.empty())
					{
						int64_t sum = std::accumulate(currLineValues.begin(), currLineValues.end(), int64_t(0));
						colLines.push_back(Line{currLineValues.size(), sum / int64_t{currLineValues.size()}});
						currLineValues.clear();
					}
				}
			}

			if (!colLines.empty())
			{
				colBestLines[i] = *std::max_element(colLines.begin(), colLines.end(), compareLineLength);
			}
			else
			{
				colBestLines[i] = Line{0, 0};
			}
		}

		auto compareLeftLine = [](Line lho, Line rho)
		{
			return (lho.averageGradient * lho.length) < (rho.averageGradient * rho.length);
		};

		auto compareRightLine = [](Line lho, Line rho)
		{
			return (-lho.averageGradient * lho.length) < (-rho.averageGradient * rho.length);
		};

		for (;;)
		{
			roboLeft = (std::max_element(colBestLines.begin(), colBestLines.end(), compareLeftLine) - colBestLines.begin()) * columnWidth;
			roboRight = (std::max_element(colBestLines.begin(), colBestLines.end(), compareRightLine) - colBestLines.begin()) * columnWidth;

			if (roboLeft > roboRight)
			{
				if (roboLeft > (columns - roboRight)) //remove from outside to inside
				{
					colBestLines[roboLeft]= Line{0, 0};
				}
				else
				{
					colBestLines[roboRight]= Line{0, 0};
				}
			}
			else
			{
				break;
			}
		}
	}

	template <typename TTmp, typename FnFilterPixel>
	void applyFilter(FnFilterPixel fn)
	{
		std::vector<TTmp> imgTmp;
		imgTmp.resize(frameLen);
		applyFilter(imgTmp, fn);
	}

	template <typename TTmp, typename FnFilterPixel>
	void applyFilter(std::vector<TTmp>& imgTmp, FnFilterPixel fn)
	{
		for (uint16_t y = 1; y < height - 1; ++y)
		{
			for (uint16_t x = 1; x < width - 1; ++x)
			{
				px(imgTmp.data(), x, y) = fn(x, y);
			}
		}

		convertToNormalizedGreyValues(imgTmp.data(), [&](uint16_t x, uint16_t y, uint8_t val)
		{
			px(x, y) = val;
		});
	}

	void render(Renderer* renderer, bool flush)
	{
		renderer->renderBA81(RENDER_FLAG_BLEND | (flush ? RENDER_FLAG_FLUSH : 0), width, height, frameLen, frame);
	}

	template <typename Fn>
	void renderDirect(Renderer* renderer, uint8_t renderFlags, Fn fn)
	{
		renderer->renderDirect(renderFlags, width, height, fn);
	}

	void debugRenderRoboPos(QPainter* p)
	{
		p->setPen(QPen(QColor(0x00, 0x00, 0xff, 0xff)));
		p->drawLine(roboLeft, 0, roboLeft, height);
		p->setPen(QPen(QColor(0xff, 0x00, 0x00, 0xff)));
		p->drawLine(roboRight, 0, roboRight, height);
	}

	template <typename T, typename Fn>
	void convertToNormalizedGreyValues(T* frame, Fn fn)
	{
		auto min = std::abs(*std::min_element(frame, frame + frameLen));
		auto max = *std::max_element(frame, frame + frameLen);

		auto makePositive = [&](int32_t val)
		{
			return (val + min) * std::numeric_limits<uint8_t>::max() / (min + max);
		};

		forEachPixel([&](uint16_t x, uint16_t y)
		{
			fn(x, y, makePositive(px(frame, x, y)));
		});
	}

	void debugRenderSobel(QPainter* p)
	{
		convertToNormalizedGreyValues(sobelFiltered.data(), [&](uint16_t x, uint16_t y, uint8_t val)
		{
			p->setPen(QPen(QColor(val, val, val, 0xff)));
			p->drawPoint(x, y);
		});
	}

private:
	uint16_t width;
	uint16_t height;
	uint32_t frameLen;
	uint8_t *frame;

	std::vector<int16_t> sobelFiltered;
	uint16_t roboLeft = 0;
	uint16_t roboRight = 0;
};

RoboModule::RoboModule(Interpreter *interpreter)
	: MonModule(interpreter)
{
}

bool RoboModule::render(uint32_t fourcc, const void *args[])
{
    if (fourcc==FOURCC('C','M','V','2'))
	{
		//uint8_t renderFlags = *(uint8_t *)args[0];
		FrameAccess frameAccess(*(uint16_t *)args[1], *(uint16_t *)args[2], *(uint32_t *)args[3], (uint8_t *)args[4]);

		//m_renderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);

		frameAccess.render(m_renderer, false);

		frameAccess.convertToGrayscale();
		frameAccess.histEqu();
		//frameAccess.gaussianFilter();
		//frameAccess.medianFilter();

		constexpr bool showSobel = false;

		frameAccess.renderDirect(m_renderer, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
		{
			if (!showSobel)
			{
				frameAccess.forEachPixel([&](uint16_t x, uint16_t y)
				{
					auto l = frameAccess.px(x, y);
					p->setPen(QPen(QColor(l, l, l, 0xff)));
					p->drawPoint(x, y);
				});
			}
			frameAccess.nonPlateFilter();
			frameAccess.sobelFilterDlDy();
			if (showSobel)
			{
				frameAccess.debugRenderSobel(p);
			}

			frameAccess.findRoboAfterSobel();
			frameAccess.debugRenderRoboPos(p);
		});

		return true;
	}
    return false;
}

bool RoboModule::command(const QStringList &argv)
{
    return false;
}

void RoboModule::paramChange()
{
}

