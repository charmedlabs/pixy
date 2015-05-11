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
#include "dataexport.h"
#include "qqueue.h"
#include "monparameterdb.h"
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
	explicit FrameAccess(uint16_t width, uint16_t height, uint8_t *frame)
		: edgeFiltered(width, height)
		, frame(width, height, frame)
	{
	}

	RawFrameAccess<uint8_t>& raw()
	{
		return frame;
	}

	uint8_t r(uint16_t x, uint16_t y)
	{
		auto pixel = &frame.px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				return *pixel;
			}
			else
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y)+frame.px(x + 1, y))>>1;
			}
		}
		else
		{
			if (x&1)
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x, y - 1)+frame.px(x, y + 1))>>1;
			}
			else
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y - 1)+frame.px(x + 1, y - 1)+frame.px(x - 1, y + 1)+frame.px(x + 1, y + 1))>>2;
			}
		}
	}

	uint8_t g(uint16_t x, uint16_t y)
	{
		auto pixel = &frame.px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y)+frame.px(x + 1, y)+frame.px(x, y + 1)+frame.px(x, y - 1))>>2;
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
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y)+frame.px(x + 1, y)+frame.px(x, y + 1)+frame.px(x, y - 1))>>2;
			}
		}
	}

	uint8_t b(uint16_t x, uint16_t y)
	{
		auto pixel = &frame.px(x, y);
		if (y&1)
		{
			if (x&1)
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y - 1)+frame.px(x + 1, y - 1)+frame.px(x - 1, y + 1)+frame.px(x + 1, y + 1))>>2;
			}
			else
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x, y - 1)+frame.px(x, y + 1))>>1;
			}
		}
		else
		{
			if (x&1)
			{
				if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
				return (frame.px(x - 1, y)+frame.px(x + 1, y))>>1;
			}
			else
			{
				return *pixel;
			}
		}
	}


	void writeToFile()
	{
		frame.writeToFile();
		{
			QImage img(frame.getWidth(), frame.getHeight(), QImage::Format_RGB32);
			frame.forEach([&](uint16_t x, uint16_t y)
			{
				img.setPixel(x, y, QColor(r(x, y), g(x, y), b(x, y)).rgb());
			});
			QString filename = uniqueFilename(MonParameterDB::docPath(), "image", "png");
			img.save(filename);
		}
	}

	uint8_t lumina(uint16_t x, uint16_t y)
	{
		return 0.2126 * r(x, y) + 0.7152 * g(x, y) + 0.0722 * b(x, y);
	}

	void convertToGrayscale()
	{
		frame.applyFilter([&](uint16_t x, uint16_t y)
		{
			return lumina(x, y);
		});
	}

	void histEqu()
	{
		std::array<uint32_t, std::numeric_limits<uint8_t>::max()> histData;
		for (auto&& entry : histData)
			entry = 0;

		frame.forEach([&](uint16_t x, uint16_t y)
		{
			++histData[frame.px(x, y)];
		});

		uint32_t sumHist = frame.getWidth() * frame.getHeight();

		std::array<uint32_t, std::numeric_limits<uint8_t>::max()> cumHistData;
		cumHistData[0] = histData[0];
		for (size_t i = 1; i < std::numeric_limits<uint8_t>::max(); ++i)
		{
			cumHistData[i] = cumHistData[i - 1] + histData[i];
		}

		frame.forEach([&](uint16_t x, uint16_t y)
		{
			frame.px(x, y) = (cumHistData[frame.px(x, y)] * std::numeric_limits<uint8_t>::max()) / sumHist;
		});
	}

	void nonPlateFilter()
	{
		constexpr auto nonPlatePercent = 0.75;
		constexpr auto nonSafetyFactor = 1;

		const auto accumulatedPixels = ((frame.getHeight() * (1 - nonPlatePercent)) * frame.getWidth());

		std::vector<uint8_t> relevantPixels;
		relevantPixels.reserve(accumulatedPixels);

		for (uint16_t y = frame.getHeight() * nonPlatePercent; y < frame.getHeight(); ++y)
		{
			for (uint16_t x = 0; x < frame.getWidth(); ++x)
			{
				relevantPixels.push_back(frame.px(x, y));
			}
		}
		std::sort(relevantPixels.begin(), relevantPixels.end());
		auto medianBrightness = relevantPixels[relevantPixels.size() / 2];

		uint8_t brightnessLimit = medianBrightness / nonSafetyFactor;

		frame.applyFilter([&](uint16_t x, uint16_t y)
		{
			auto val = frame.px(x, y);
			return val > brightnessLimit ? 255 : val;
		});
	}

	void gaussianFilter()
	{
		frame.applyFilter([&](uint16_t x, uint16_t y)
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0.;
			return (
						frame.px(x-1, y-1) / 16. +
						frame.px(x,   y-1) / 8. +
						frame.px(x+1, y-1) / 16. +

						frame.px(x-1, y) / 8. +
						frame.px(x,   y) / 4. +
						frame.px(x+1, y) / 8. +

						frame.px(x-1, y+1) / 16. +
						frame.px(x,   y+1) / 8. +
						frame.px(x+2, y+1) / 16.

						);
		});
	}

	void gaussianFilterOnEdges()
	{
		frame.applyFilter(edgeFiltered, [&](uint16_t x, uint16_t y)
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0.;
			return (
						edgeFiltered.px(x-1, y-1) / 16. +
						edgeFiltered.px(x,   y-1) / 8. +
						edgeFiltered.px(x+1, y-1) / 16. +

						edgeFiltered.px(x-1, y) / 8. +
						edgeFiltered.px(x,   y) / 4. +
						edgeFiltered.px(x+1, y) / 8. +

						edgeFiltered.px(x-1, y+1) / 16. +
						edgeFiltered.px(x,   y+1) / 8. +
						edgeFiltered.px(x+2, y+1) / 16.

						);
		});
	}

	void medianFilter()
	{
		frame.applyFilter([&](uint16_t x, uint16_t y)->uint8_t
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
			std::vector<uint8_t> values =
			{
				frame.px(x-1, y-1),
				frame.px(x,   y-1),
				frame.px(x+1, y-1),

				frame.px(x-1, y),
				frame.px(x,   y),
				frame.px(x+1, y),

				frame.px(x-1, y+1),
				frame.px(x,   y+1),
				frame.px(x+1, y+1)
			};
			std::sort(values.begin(), values.end());
			return values[values.size() / 2];
		});
	}

	void sobelFilterDlDx()
	{
		frame.applyFilter(edgeFiltered, [&](uint16_t x, uint16_t y)
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
			return (
						-1 * frame.px(x-1, y-1) +
						-2 * frame.px(x,   y-1) +
						-1 * frame.px(x+1, y-1) +

						1 * frame.px(x-1, y+1) +
						2 * frame.px(x,   y+1) +
						1 * frame.px(x+1, y+1)

						);
		});
	}

	void sobelFilterDlDy()
	{
		frame.applyFilter(edgeFiltered, [&](uint16_t x, uint16_t y)
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
			return (
						-1 * int32_t(frame.px(x-1, y-1)) +
						-2 * int32_t(frame.px(x,   y-1)) +
						-1 * int32_t(frame.px(x+1, y-1)) +

						0 * int32_t(frame.px(x-1, y)) +
						0 * int32_t(frame.px(x,   y)) +
						0 * int32_t(frame.px(x+1, y)) +

						1 * int32_t(frame.px(x-1, y+1)) +
						2 * int32_t(frame.px(x,   y+1)) +
						1 * int32_t(frame.px(x+1, y+1))

						);
		});
	}

	void laplaceFilter()
	{
		frame.applyFilter(edgeFiltered, [&](uint16_t x, uint16_t y)
		{
			if (x == 0 || y == 0||x==frame.getWidth()||y==frame.getHeight()) return 0;
			return (
						0 * frame.px(x-1, y-1) +
						-1 * frame.px(x,   y-1) +
						0 * frame.px(x+1, y-1) +

						-1 * frame.px(x-1, y) +
						4 * frame.px(x,   y) +
						-1 * frame.px(x+1, y) +

						0 * frame.px(x-1, y+1) +
						-1 * frame.px(x,   y+1) +
						0 * frame.px(x+1, y+1)
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

		const auto columns = frame.getWidth() / columnWidth;

		std::vector<std::vector<int16_t>> colValues;
		colValues.resize(columns);

		auto h = frame.getHeight(); //circumvent compiler bug
		frame.forEach([&](uint16_t x, uint16_t y)
		{
			if (x >= 5 && x < (frame.getWidth()-5) &&  //minus 5 pixels left and right (various filters)
				x >= frame.getWidth() * percentToStripFromEachSide &&
				x <= frame.getWidth() * (1 - percentToStripFromEachSide))
			{
				if (colValues[x / columnWidth].empty())
				{
					auto& vec = colValues[x / columnWidth];
					vec.resize(h);
				}
				colValues[x / columnWidth][y] += edgeFiltered.px(x, y);
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
			for (auto y = frame.getHeight() * minDistFromTopPercent; y < colValues[i].size(); ++y)
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

	void render(Renderer* renderer, bool flush)
	{
		renderer->renderBA81(RENDER_FLAG_BLEND | (flush ? RENDER_FLAG_FLUSH : 0), frame.getWidth(), frame.getHeight(), frame.getFrameLen(), frame.getData());
	}

	template <typename Fn>
	void renderDirect(Renderer* renderer, uint8_t renderFlags, Fn fn)
	{
		renderer->renderDirect(renderFlags, frame.getWidth(), frame.getHeight(), fn);
	}

	void debugRenderRoboPos(QPainter* p)
	{
		p->setPen(QPen(QColor(0x00, 0x00, 0xff, 0xff)));
		p->drawLine(roboLeft, 0, roboLeft, frame.getHeight());
		p->setPen(QPen(QColor(0xff, 0x00, 0x00, 0xff)));
		p->drawLine(roboRight, 0, roboRight, frame.getHeight());
	}

	template <typename Y, typename Fn>
	void convertToNormalizedGreyValues(Y& frame, Fn fn)
	{
		std::vector<uint32_t> elements(frame.getData(), frame.getData() + frame.getFrameLen());
		std::sort(elements.begin(), elements.end());
		auto tenPercent = elements.size() * 0.2;
		elements.erase(elements.begin(), elements.begin() + tenPercent);
		elements.erase(elements.end()-tenPercent, elements.end());
		auto min = *std::min_element(elements.begin(), elements.end());
		auto max = *std::max_element(elements.begin(), elements.end());

		auto range = max - min;
		auto factor = static_cast<double>(std::numeric_limits<uint8_t>::max()) / static_cast<double>(range);
		auto offset = -min;

		auto makePositive = [&](int32_t val)
		{
			return (val+offset)*factor;
		};

		frame.forEach([&](uint16_t x, uint16_t y)
		{
			auto orig = frame.px(x, y);
			auto val = makePositive(orig);
			fn(x, y, val);
		});
	}

	void debugRenderEdges(QPainter* p)
	{
		convertToNormalizedGreyValues(edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
		{
			p->setPen(QPen(QColor(val, val, val, 0xff)));
			p->drawPoint(x, y);
		});
	}

public:
	RawFrame<int32_t> edgeFiltered;

private:
	RawFrameAccess<uint8_t> frame;

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
		ASSERT(*(uint32_t *)args[3] == uint32_t(*(uint16_t *)args[1] * *(uint16_t *)args[2]));
		RawFrameAccess<uint8_t> frameAccess(*(uint16_t *)args[1], *(uint16_t *)args[2], (uint8_t *)args[4]);

		static uint8_t no = 0;
		if (++no % 16 == 1)
		{
			//frameAccess.writeToFile();
		}

		debugRender(nullptr, m_interpreter->m_renderer, frameAccess);

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

void RoboModule::debugRender(std::function<Renderer*()> makeRenderer, Renderer* pRenderer, RawFrameAccess<uint8_t>& rawFrame, RoboParameters params)
{

	//pRenderer->renderBA81(RENDER_FLAG_BLEND, width, height, frameLen, frame);

	FrameAccess frameAccess(rawFrame.getWidth(), rawFrame.getHeight(), rawFrame.getData());
	frameAccess.render(pRenderer, false);

	enum class Type
	{
		ShowColor,
		ShowGrayscale,
		ShowLaplaceFiltered,
		ShowProcessed,
		ShowNew
	};

	constexpr Type type = Type::ShowNew;

	if (type == Type::ShowColor)
	{
		frameAccess.renderDirect(pRenderer, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
		{
			frameAccess.raw().forEach([&](uint16_t x, uint16_t y)
			{
				p->setPen(QPen(QColor(frameAccess.r(x, y), frameAccess.g(x, y), frameAccess.b(x, y), 0xff)));
				p->drawPoint(x, y);
			});
		});
	}
	else if (type == Type::ShowGrayscale)
	{
		frameAccess.convertToGrayscale();
		frameAccess.renderDirect(pRenderer, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
		{
			frameAccess.raw().forEach([&](uint16_t x, uint16_t y)
			{
				auto l = frameAccess.raw().px(x, y);
				p->setPen(QPen(QColor(l, l, l, 0xff)));
				p->drawPoint(x, y);
			});
		});
	}
	else if (type == Type::ShowLaplaceFiltered)
	{
		frameAccess.convertToGrayscale();
		frameAccess.histEqu();
		frameAccess.medianFilter();

		frameAccess.renderDirect(pRenderer, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
		{
			frameAccess.laplaceFilter();
			frameAccess.debugRenderEdges(p);
		});
	}
	else if (type == Type::ShowProcessed)
	{
		constexpr bool showEdge = true;
		frameAccess.convertToGrayscale();
		frameAccess.histEqu();
		frameAccess.gaussianFilter();
		frameAccess.medianFilter();


		frameAccess.renderDirect(pRenderer, RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
		{
			if (!showEdge)
			{
				frameAccess.raw().forEach([&](uint16_t x, uint16_t y)
				{
					auto l = frameAccess.raw().px(x, y);
					p->setPen(QPen(QColor(l, l, l, 0xff)));
					p->drawPoint(x, y);
				});
			}
			frameAccess.nonPlateFilter();
			frameAccess.sobelFilterDlDy();
			//frameAccess.laplaceFilter();
			if (showEdge)
			{
				frameAccess.debugRenderEdges(p);
			}

			frameAccess.findRoboAfterSobel();
			frameAccess.debugRenderRoboPos(p);
		});
	}
	else if (type == Type::ShowNew)
	{
		frameAccess.render(pRenderer, true);

		frameAccess.convertToGrayscale();
		frameAccess.gaussianFilter();
		frameAccess.sobelFilterDlDy();
		RawFrame<bool> binaryImage(frameAccess.raw().getWidth(), frameAccess.raw().getHeight());


		std::vector<uint32_t> allPlatePixels(frameAccess.raw().getWidth());

		auto renderBinary = [&](RawFrame<bool>& binaryImage)
		{
			if (makeRenderer)
			{
				frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
				{
					binaryImage.forEach([&](uint16_t x, uint16_t y)
					{
						auto state = binaryImage.px(x, y);
						auto color = state ? 0xff : 0x00;
						p->setPen(QPen(QColor(color, color, color, 0xff)));
						p->drawPoint(x, y);
						if (allPlatePixels[x] >= 20 && allPlatePixels[x] == (frameAccess.raw().getHeight() - y))
						{
							p->setPen(QPen(QColor(0xff, 0x00, 0x00, 0xff)));
							p->drawPoint(x, y);
						}
					});
				});
			}
		};

		if (makeRenderer)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.convertToNormalizedGreyValues(frameAccess.edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
				{
					p->setPen(QPen(QColor(val, val, val, 0xff)));
					p->drawPoint(x, y);
				});
			});
		}

		frameAccess.gaussianFilterOnEdges();

		frameAccess.edgeFiltered.applyFilter(binaryImage, [&](uint16_t x, uint16_t y)
		{
			auto value = frameAccess.edgeFiltered.px(x, y);
			return value < -params.limit;
			return value > params.limit || value < -params.limit;
		});

		if (makeRenderer && false)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.convertToNormalizedGreyValues(frameAccess.edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
				{
					p->setPen(QPen(QColor(val, val, val, 0xff)));
					p->drawPoint(x, y);
				});
			});
		}


		std::vector<uint32_t> unfilteredAllPlatePixels(frameAccess.raw().getWidth());
		auto topBottomBorder = 10;
		for (auto x = topBottomBorder + 1; x < frameAccess.raw().getWidth() - topBottomBorder; ++x)
		{
			auto platePixels = 10;
			for (auto y = platePixels; y < binaryImage.getHeight(); ++y)
			{
				if (!binaryImage.px(x, binaryImage.getHeight() - y))
				{
					platePixels += 1;
				}
				else
				{
					break;
				}
			}

			unfilteredAllPlatePixels[x] = (platePixels < (binaryImage.getHeight()-topBottomBorder) && platePixels >= topBottomBorder) ? platePixels : 0;
		}

		constexpr auto averagingFrame = 20;
		constexpr auto averagingLimit = 20;
		for (auto x = 0; x < frameAccess.raw().getWidth(); ++x)
		{
			int32_t left = x - averagingFrame;
			int32_t right = x + averagingFrame;
			left = std::min(0, left);
			right = std::max(uint16_t{0}, frameAccess.raw().getWidth());

			auto sum = std::accumulate(unfilteredAllPlatePixels.begin() + left, unfilteredAllPlatePixels.begin() + right, 0);
			auto avg = sum / (right - left);

			allPlatePixels[x] = (std::abs(unfilteredAllPlatePixels[x] - avg) < averagingLimit) ? unfilteredAllPlatePixels[x] : 0;
		}

		renderBinary(binaryImage);
		frameAccess.laplaceFilter();
		if (makeRenderer && false)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.convertToNormalizedGreyValues(frameAccess.edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
				{
					p->setPen(QPen(QColor(val, val, val, 0xff)));
					p->drawPoint(x, y);
				});
			});
		}
		frameAccess.gaussianFilterOnEdges();
		if (makeRenderer && false)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.convertToNormalizedGreyValues(frameAccess.edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
				{
					p->setPen(QPen(QColor(val, val, val, 0xff)));
					p->drawPoint(x, y);
				});
			});
		}

		frameAccess.sobelFilterDlDx();
		if (makeRenderer && false)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.convertToNormalizedGreyValues(frameAccess.edgeFiltered, [&](uint16_t x, uint16_t y, uint8_t val)
				{
					p->setPen(QPen(QColor(val, val, val, 0xff)));
					p->drawPoint(x, y);
				});
			});
		}

		RawFrame<bool> binaryImage2(frameAccess.raw().getWidth(), frameAccess.raw().getHeight());
		frameAccess.edgeFiltered.applyFilter(binaryImage2, [&](uint16_t x, uint16_t y)
		{
			auto value = frameAccess.edgeFiltered.px(x, y);
			//return value < -params.xLimit;
			return value > params.xLimit || value < -params.xLimit;
		});
		renderBinary(binaryImage2);

		RawFrame<bool> binaryImage3(frameAccess.raw().getWidth(), frameAccess.raw().getHeight());
		binaryImage3.applyFilter(binaryImage3, [&](uint16_t x, uint16_t y)
		{
			bool isUnderBorder = (allPlatePixels[x] >= 20) && ((allPlatePixels[x] - topBottomBorder) > (frameAccess.raw().getHeight() - y));
			if (isUnderBorder)
			{
				return binaryImage2.px(x, y);
			}
			else
			{
				return false;
			}
		});
		renderBinary(binaryImage3);


		std::vector<uint32_t> roboPixels(frameAccess.raw().getWidth());
		binaryImage3.forEach([&](uint16_t x, uint16_t y)
		{
			if (binaryImage3.px(x, y))
			{
				roboPixels[x] += 1;
			}
		});

		if (makeRenderer)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				binaryImage3.forEach([&](uint16_t x, uint16_t y)
				{
					p->setPen(QPen(QColor(0xff, 0xff, 0xff, 0xff)));
					p->drawPoint(x, y);
					if (roboPixels[x] > (frameAccess.raw().getHeight() - y))
					{
						p->setPen(QPen(QColor(0xff, 0x00, 0x00, 0xff)));
						p->drawPoint(x, y);
					}
				});
			});
		}

		auto column = 0;
		auto maxElement = *std::max_element(roboPixels.begin(), roboPixels.end());
		for (auto x = 0; x < roboPixels.size(); ++x)
		{
			if (roboPixels[x] == maxElement)
			{
				column = x;
			}
		}

		if (makeRenderer)
		{
			frameAccess.renderDirect(makeRenderer(), RENDER_FLAG_BLEND | RENDER_FLAG_FLUSH, [&](QPainter* p)
			{
				frameAccess.raw().forEach([&](uint16_t x, uint16_t y)
				{
					p->setPen(QPen(QColor(frameAccess.r(x, y), frameAccess.g(x, y), frameAccess.b(x, y), 0xff)));
					p->drawPoint(x, y);
					if (x > column - 5 && x < column + 5)
					{
						p->setPen(QPen(QColor(0xff, 0x00, 0x00, 0xff)));
						p->drawPoint(x, y);
					}
				});
			});
		}
	}
}

