#include "interpreter.h"
#include "sigeditdialog.h"
#include "ui_sigeditdialog.h"

#include <QPainter>
#include <QImage>
#include <QPaintEngine>
#include <QPalette>
#include <QMouseEvent>
#include <QAbstractButton>
#include <QDebug>
#include "renderer.h"
#include "calc.h"
#include "chirp.hpp"
#include "string.h"

// COLORSPACE UTILS ***********************************************************

#define UV_SCALE_MAX ((0xFF << CL_LUT_ENTRY_SCALE) / 0xFF)

inline void rgb2yuv(uint32_t rgb, int32_t *y, int32_t *u, int32_t *v) {
    int r = (rgb >> 16) & 0xFF;
    int g = (rgb >> 8) & 0xFF;
    int b = rgb & 0xFF;
    *y = r + g + b;
    if (*y == 0) {
        *u = *v = 0;
    }
    else {
        *u = ((r - g) << CL_LUT_ENTRY_SCALE) / *y;
        *v = ((b - g) << CL_LUT_ENTRY_SCALE) / *y;
    }
}
inline int32_t uv2rgb(int32_t u, int32_t v) {
    int32_t y;
    int yu, yv, r, g, b;
    for (y = (0xFF * 2); y >= 0; y--) {
        yu = (u * y) >> CL_LUT_ENTRY_SCALE;
        yv = (v * y) >> CL_LUT_ENTRY_SCALE;
        g = (y - (yu + yv)) / 3;
        r = yu + g;
        b = yv + g;
        if ((r >= 0x00) && (g >= 0x00) && (b >= 0x00) &&
            (r <= 0xFF) && (g <= 0xFF) && (b <= 0xFF)) {
            return((r << 16) | (g << 8) | b);
        }
    }
    return(0);
}

// return whether two signatures have equivalent core properties
inline bool sigsEqual(ColorSignature s1, ColorSignature s2) {
    return((s1.m_uMin == s2.m_uMin) &&
           (s1.m_uMean == s2.m_uMean) &&
           (s1.m_uMax == s2.m_uMax) &&
           (s1.m_vMin == s2.m_vMin) &&
           (s1.m_vMean == s2.m_vMean) &&
           (s1.m_vMax == s2.m_vMax));
}

inline bool sigIsEmpty(ColorSignature s) {
    return((s.m_uMin == 0) && (s.m_uMax == 0) &&
           (s.m_vMin == 0) && (s.m_vMax == 0));
}

// DIALOG *********************************************************************

SigEditDialog::SigEditDialog(QWidget *parent, Interpreter *interpreter) :
    QDialog(parent),
    ui(new Ui::SigEditDialog)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(clicked(QAbstractButton*)), this, SLOT(apply(QAbstractButton*)));

    // bind to the interpreter
    m_interpreter = interpreter;
    m_interpreter->unwait(); // unhang interpreter if it's waiting

    // receive video frames from the renderer
    m_framecount = 0;
    Renderer *renderer = m_interpreter->m_renderer;
    connect(renderer, SIGNAL(image(QImage, uchar)),
            this, SLOT(handleImage(QImage,uchar)));

    // track hover events to change the cursor
    setMouseTracking(true);
    ui->mapFrame->setAttribute(Qt::WA_TransparentForMouseEvents);
    m_dragging = false;
    m_dragIndex = m_selectIndex = -1;

    // save initial signatures and ranges
    storeSignatures();

    show();
}

void SigEditDialog::handleImage(QImage image, uchar renderFlags)
{
    // simulate the image list in videowidget.cpp so we only get
    //  the background video
    if (! (renderFlags & RENDER_FLAG_BLEND)) m_framecount = 0;
    m_framecount++;
    if (m_framecount == 1) {
        // set up the map image
        int size = 512;
        QImage map(size, size, QImage::Format_ARGB32);
        map.fill(QColor(0, 0, 0, 0)); // transparency
        QPainter painter;
        painter.begin(&map);
        painter.setPen(Qt::NoPen);
        QRect spot;
        // transfer the camera image onto the colorspace map
        int px, py;
        int32_t y, u, v;
        int32_t yMin = SigModule::getYMin();
        uint *p;
        for (py = 0; py < image.height(); py++) {
            p = (uint *)image.scanLine(py);
            for (px = 0; px < image.width(); px++) {
                // project into the colorspace
                rgb2yuv(*p, &y, &u, &v);
                // scale onto the image
                u = ((u + UV_SCALE_MAX) * size) / (UV_SCALE_MAX * 2);
                v = ((v + UV_SCALE_MAX) * size) / (UV_SCALE_MAX * 2);
                // draw a spot on the map
                if (y >= yMin) {
                    spot = QRect(u - 2, v - 2, 4, 4);
                    painter.fillRect(spot, *p);
                }
                p++;
            }
        }
        painter.end();
        // add the image to the stack
        m_images.push_back(map);
        while (m_images.count() > MAX_IMAGES) m_images.pop_front();
        // redraw to show the new image
        update(rect());
    }
    // clear the simulated image list if asked to flush
    if (renderFlags & RENDER_FLAG_FLUSH) m_framecount = 0;
}

void SigEditDialog::paintEvent(QPaintEvent *event)
{
    int i;
    QPainter painter(this);
    QPixmap pixmap;
    QRect xybounds = ui->mapFrame->geometry();
    // map coordinates into (u, v) space
    painter.translate(xybounds.center().x(), xybounds.center().y());
    qreal xscale = (qreal)xybounds.width() / (qreal)(2 * UV_SCALE_MAX);
    qreal yscale = (qreal)xybounds.height() / (qreal)(2 * UV_SCALE_MAX);
    painter.scale(xscale, yscale);
    QRect uvbounds = QRect(QPoint(- UV_SCALE_MAX, - UV_SCALE_MAX),
                     QPoint(UV_SCALE_MAX, UV_SCALE_MAX));
    // draw the background
    paintBackground(painter);
    // draw the stack of colorspace map frames
    for (i = 0; i < m_images.count(); i++) {
        pixmap.convertFromImage(m_images[i]);
        painter.drawPixmap(uvbounds, pixmap);
    }
    // draw signature boxes
    for (i = 0; i < CL_NUM_SIGNATURES; i++) {
        paintSignature(painter, i);
    }
}

// draw a background showing the extents of the uv prjection
//  with reference colors at the edges
void SigEditDialog::paintBackground(QPainter &painter)
{
    int i, j, y, u, v;
    uint32_t refcolors[6] = { 0xFF0000, 0xFFFF00, 0x00FF00,
                              0x00FFFF, 0x0000FF, 0xFF00FF };
    QPoint corners[6];
    painter.setPen(Qt::NoPen);
    for (i = 0; i < 6; i++) {
        rgb2yuv(refcolors[i], &y, &u, &v);
        corners[i] = QPoint(u, v);
    }
    // draw the background
    painter.setBrush(QColor(0x80, 0x80, 0x80));
    painter.drawPolygon(corners, 6);
    // draw the edge of the colorspace to show the reference colors
    painter.setBrush(Qt::NoBrush);
    QPen edgePen;
    j = 5;
    for (i = 0; i < 6; i++) {
        QLinearGradient grad = QLinearGradient(
            QPointF(corners[i]), QPointF(corners[j]));
        grad.setColorAt(0.0, refcolors[i]);
        grad.setColorAt(1.0, refcolors[j]);
        edgePen = QPen(QBrush(grad), 3);
        edgePen.setCosmetic(true);
        edgePen.setCapStyle(Qt::RoundCap);
        painter.setPen(edgePen);
        painter.drawLine(corners[i], corners[j]);
        j = i;
    }
}

// draw a box for a signature
void SigEditDialog::paintSignature(QPainter &painter, int i)
{
    ColorSignature sig = SigModule::getSignature(i);
    // skip empty ones
    if (sigIsEmpty(sig)) return;
    QRect main = sigRect(sig);
    QRect scaled = effectiveSigRect(sig, SigModule::getRange(i));
    QRect outer = scaled.united(main);
    // set up signature style
    QColor sigColor = QColor(255, 255, 255);
    if ((i == m_dragIndex) || (i == m_selectIndex)) {
        sigColor = QApplication::palette().color(QPalette::Highlight);
    }
    QPen sigPen = QPen(sigColor, 2);
    sigPen.setCosmetic(true);
    sigColor.setAlphaF(0.25);
    QBrush sigBrush = QBrush(sigColor);
    // draw the stored area of the signature
    painter.setBrush(Qt::NoBrush);
    painter.setPen(sigPen);
    painter.drawRect(main);
    // draw the area after applying the scale factor
    painter.setBrush(sigBrush);
    painter.setPen(Qt::NoPen);
    painter.drawRect(scaled);
    // draw a label for the signature
    painter.setPen(sigPen);
    painter.setBrush(Qt::NoBrush);
    QString label;
    label.sprintf("%d", i + 1);
    QFont font = painter.font();
    font.setPixelSize(qMin((int)(14.0 / painter.transform().m22()), outer.height()));
    font.setBold(true);
    painter.setFont(font);
    painter.drawText(outer, Qt::AlignHCenter | Qt::AlignVCenter, label);
}

SigEditDialog::~SigEditDialog()
{
    delete ui;
}

void SigEditDialog::storeSignatures()
{
    for (int i = 0; i < CL_NUM_SIGNATURES; i++) {
        m_initSignatures[i] = SigModule::getSignature(i);
        m_initRanges[i] = SigModule::getRange(i);
    }
}
void SigEditDialog::recallSignatures() {
    for (int i = 0; i < CL_NUM_SIGNATURES; i++) {
        SigModule::instance->updateSignature(i, m_initSignatures[i], m_initRanges[i]);
    }
}

void SigEditDialog::apply(QAbstractButton *button)
{
    if (button->text() == "Apply") {
        storeSignatures();
        m_interpreter->saveParams();
    }
}
void SigEditDialog::accept()
{
    m_interpreter->saveParams();
    QDialog::accept();
}
void SigEditDialog::reject()
{
    recallSignatures();
    m_interpreter->saveParams(true);
    QDialog::reject();
}

// INTERACTION ****************************************************************

void SigEditDialog::mouseMoveEvent(QMouseEvent *event)
{
    // convert the mouse position into colorspace coordinates
    int32_t u, v;
    xy2uv(event->pos(), &u, &v);
    // if we're dragging, move stuff
    if (m_dragging) updateDrag(u, v);
    else updateHover(u, v);
    update();
}
void SigEditDialog::mousePressEvent(QMouseEvent *event)
{
    m_selectIndex = m_dragIndex;
    if (event->button() == Qt::LeftButton) m_dragging = true;
    update();
}
void SigEditDialog::mouseReleaseEvent(QMouseEvent *event)
{
    m_dragging = false;
    mouseMoveEvent(event);
    update();
}
void SigEditDialog::keyPressEvent(QKeyEvent *event)
{
    if (m_selectIndex >= 0) {
        if ((event->key() == Qt::Key_Delete) ||
            (event->key() == Qt::Key_Backspace)) {
            SigModule::instance->deleteSignature(m_selectIndex);
            m_selectIndex = -1;
        }
    }
    QDialog::keyPressEvent(event);
}

// get the stored area of a signature, not including scale factor
QRect SigEditDialog::sigRect(ColorSignature sig)
{
    return(QRect(QPoint(sig.m_uMin, sig.m_vMin),
                 QPoint(sig.m_uMax, sig.m_vMax)));
}

// get the effective area of a signature, including scale factor
QRect SigEditDialog::effectiveSigRect(ColorSignature sig, float range) {
    return(QRect(QPoint(
            sig.m_uMean + (sig.m_uMin - sig.m_uMean) * range,
            sig.m_vMean + (sig.m_vMin - sig.m_vMean) * range
        ), QPoint(
            sig.m_uMean + (sig.m_uMax - sig.m_uMean) * range,
            sig.m_vMean + (sig.m_vMax - sig.m_vMean) * range
        )));
}

// get the outer area of a signature for dragging purposes
QRect SigEditDialog::outerSigRect(ColorSignature sig, float range)
{
    QRect r = sigRect(sig);
    return(r.united(effectiveSigRect(sig, range)));
}

// transfer properties from a rectangle onto a signature
void SigEditDialog::rect2sig(QRect r, ColorSignature *sig)
{
    r = r.normalized();
    sig->m_uMin = r.left();
    sig->m_uMean = r.center().x();
    sig->m_uMax = r.right();
    sig->m_vMin = r.top();
    sig->m_vMean = r.center().y();
    sig->m_vMax = r.bottom();
}

// transform a point from widget coordinates to uv space
void SigEditDialog::xy2uv(QPointF xy, int32_t *u, int32_t *v) {
    QRect r = ui->mapFrame->geometry();
    *u = ((xy.x() - r.center().x()) * (2 * UV_SCALE_MAX)) / r.width();
    *v = ((xy.y() - r.center().y()) * (2 * UV_SCALE_MAX)) / r.height();
}

void SigEditDialog::updateHover(int32_t u, int32_t v)
{
    int i;
    QRect outer, r;
    // get tolerances in pixels
    QRect frame = ui->mapFrame->geometry();
    int edgePixels = 4;
    int uEdgeSize = (edgePixels * (2 * UV_SCALE_MAX)) / frame.width();
    int vEdgeSize = (edgePixels * (2 * UV_SCALE_MAX)) / frame.width();
    // see if the mouse is over any of the signatures
    int emptyCount = 0;
    m_dragIndex = -1;
    ColorSignature sig;
    for (i = 0; i < CL_NUM_SIGNATURES; i++) {
        sig = SigModule::getSignature(i);
        if (sigIsEmpty(sig)) {
            emptyCount++;
            continue;
        }
        r = outer = outerSigRect(sig, SigModule::getRange(i));
        // expand the edge to make it easier to resize
        r.adjust(- (uEdgeSize / 2), - (vEdgeSize / 2),
                 uEdgeSize / 2, vEdgeSize / 2);
        // determine the action we'd be taking if the mouse was pressed
        m_moving = false;
        m_uOffset = m_vOffset = 0;
        if (r.contains(u, v, false)) {
            if (u - r.left() <= uEdgeSize) m_uOffset = -1;
            else if (r.right() - u <= uEdgeSize) m_uOffset = 1;
            if (v - r.top() <= vEdgeSize) m_vOffset = -1;
            else if (r.bottom() - v <= vEdgeSize) m_vOffset = 1;
            // dragging the whole signature
            if ((m_uOffset == 0) && (m_vOffset == 0)) {
                m_moving = true;
                m_uOffset = u - outer.left();
                m_vOffset = v - outer.top();
                setCursor(Qt::OpenHandCursor);
            }
            // dragging corners
            else if (m_uOffset == m_vOffset) setCursor(Qt::SizeFDiagCursor);
            else if (m_uOffset == - m_vOffset) setCursor(Qt::SizeBDiagCursor);
            // dragging edges
            else if (m_uOffset != 0) setCursor(Qt::SizeHorCursor);
            else if (m_vOffset != 0) setCursor(Qt::SizeVerCursor);
            m_dragIndex = i;
            return;
        }
    }
    // if the mouse is over the colorspace, show a crosshairs to indicate
    //  another signature can be created
    if ((u >= - UV_SCALE_MAX) && (u <= UV_SCALE_MAX) &&
        (v >= - UV_SCALE_MAX) && (v <= UV_SCALE_MAX) &&
        (emptyCount > 0)) setCursor(Qt::CrossCursor);
    // if the mouse is not over any signatures, clear the cursor
    else setCursor(Qt::ArrowCursor);
}

void SigEditDialog::updateDrag(int32_t u, int32_t v)
{
    QRect r;
    int x, y, w, h;
    int i = m_dragIndex;
    ColorSignature sig = SigModule::getSignature(i);
    float range = SigModule::getRange(i);
    // if no signature is being dragged, try to create one
    if (! (m_dragIndex >= 0)) {
        for (i = 0; i < CL_NUM_SIGNATURES; i++) {
            sig = SigModule::getSignature(i);
            if (sigIsEmpty(sig)) {
                range = 1.0;
                sig.m_uMin = sig.m_uMean = sig.m_uMax = u;
                sig.m_vMin = sig.m_vMean = sig.m_vMax = v;
                m_dragIndex = m_selectIndex = i;
                m_uOffset = m_vOffset = 1;
                m_moving = false;
                break;
            }
        }
        // if we couldn't create a signature, bail out
        if (! (m_dragIndex >= 0)) return;
    }
    // normalize the range once the signature is under control of the editor
    if (range != 1.0) {
        r = outerSigRect(sig, range);
        rect2sig(r, &sig);
        range = 1.0;
    }
    // get the current position of the signature
    r = sigRect(sig);
    // handle moving the whole signature
    if (m_moving) {
        setCursor(Qt::ClosedHandCursor);
        x = u - m_uOffset;
        y = v - m_vOffset;
        w = r.width();
        h = r.height();
        if (x < - UV_SCALE_MAX) x = - UV_SCALE_MAX;
        if (y < - UV_SCALE_MAX) y = - UV_SCALE_MAX;
        if (x + w > UV_SCALE_MAX) x = UV_SCALE_MAX - w;
        if (y + h > UV_SCALE_MAX) y = UV_SCALE_MAX - h;
        r = QRect(x, y, w, h);
    }
    // handle dragging edges and corners
    else {
        if (m_uOffset > 0) {
            r.setRight(u);
            if (r.right() < r.left()) r.setRight(r.left());
            if (r.right() > UV_SCALE_MAX) r.setRight(UV_SCALE_MAX);
        }
        else if (m_uOffset < 0) {
            r.setLeft(u);
            if (r.left() > r.right()) r.setLeft(r.right());
            if (r.left() < - UV_SCALE_MAX) r.setLeft(- UV_SCALE_MAX);
        }
        if (m_vOffset > 0) {
            r.setBottom(v);
            if (r.bottom() < r.top()) r.setBottom(r.top());
            if (r.bottom() > UV_SCALE_MAX) r.setBottom(UV_SCALE_MAX);
        }
        else if (m_vOffset < 0) {
            r.setTop(v);
            if (r.top() > r.bottom()) r.setTop(r.bottom());
            if (r.top() < - UV_SCALE_MAX) r.setTop(- UV_SCALE_MAX);
        }
    }
    // update the signature and range
    rect2sig(r, &sig);
    // update the mean and color marker
    sig.m_uMean = (sig.m_uMin + sig.m_uMax) / 2;
    sig.m_vMean = (sig.m_vMin + sig.m_vMax) / 2;
    sig.m_rgb = uv2rgb(sig.m_uMean, sig.m_vMean);
    SigModule::instance->updateSignature(i, sig, range);
}

// PIXY COMMUNICATION *********************************************************

MON_MODULE(SigModule)

SigModule *SigModule::instance;

SigModule::SigModule(Interpreter *interpreter) : MonModule(interpreter)
{
    // clear signatures and ranges
    memset(m_signatures, 0, sizeof(m_signatures));
    memset(m_ranges, 0, sizeof(m_ranges));
    m_yMin = 0;
    // store the singleton instance statically
    instance = this;
}

SigModule::~SigModule() { }

bool SigModule::render(uint32_t fourcc, const void *args[]) { return(false); }
void SigModule::paramChange()
{
    int i;
    QString id;
    QVariant val;
    QByteArray ba;
    uint32_t sigLen;
    uint8_t *sigData;
    // get all existing signatures and ranges
    for (i = 0; i < CL_NUM_SIGNATURES; i++) {
        id.sprintf("signature%d", i + 1);
        val = pixyParameter(id);
        ba = val.toByteArray();
        Chirp::deserialize((uint8_t *)ba.data(), ba.size(),
                           &sigLen, &sigData, END);
        if (sigLen == sizeof(ColorSignature)) {
            memcpy(&(m_signatures[i]), sigData, sizeof(ColorSignature));
        }
        id.sprintf("Signature %d range", i + 1);
        val = pixyParameter(id);
        if (val.canConvert(QVariant::Double)) {
            m_ranges[i] = val.toFloat();
        }
    }
    // get the minimum brightness
    val = pixyParameter("Min brightness");
    if (val.canConvert(QVariant::Double)) {
        m_yMin = val.toFloat() * (3 * 0xFF);
    }
}

ColorSignature SigModule::getSignature(int i)
{
    if (instance) return(instance->m_signatures[i]);
    return(ColorSignature());
}
float SigModule::getRange(int i)
{
    if (instance) return(instance->m_ranges[i]);
    return(1.0);
}
int32_t SigModule::getYMin()
{
    if (instance) return(instance->m_yMin);
    return(0);
}

void SigModule::updateSignature(int i, ColorSignature sig, float range)
{
    QString id;
    Parameter *parameter;
    m_interpreter->m_pixyParameters.mutex()->lock();
    if (! sigsEqual(m_signatures[i], sig)) {
        m_signatures[i] = sig;
        // serialize the signature
        QByteArray ba;
        int bufSize = sizeof(ColorSignature) + 8 + CRP_BUFPAD;
        ba.fill(0, bufSize);
        Chirp::serialize(NULL, (uint8_t *)ba.data(), bufSize, CRP_INTS8,
                         sizeof(ColorSignature), (uint8_t *)&sig, END);
        ba.chop(CRP_BUFPAD);
        // update
        id.sprintf("signature%d", i + 1);
        parameter = m_interpreter->m_pixyParameters.parameter(id);
        if (parameter) {
            parameter->set(QVariant::fromValue(ba), false);
            parameter->setDirty(true);
        }
    }
    // update the range if it's changing
    if (m_ranges[i] != range) {
        m_ranges[i] = range;
        id.sprintf("Signature %d range", i + 1);
        parameter = m_interpreter->m_pixyParameters.parameter(id);
        if (parameter) {
            parameter->set(QVariant::fromValue(range), false);
            parameter->setDirty(true);
        }
    }
    m_interpreter->m_pixyParameters.mutex()->unlock();
    m_interpreter->updateParam();
}
void SigModule::deleteSignature(int i)
{
    ColorSignature sig = m_signatures[i];
    sig.m_uMin = sig.m_uMean = sig.m_uMax = 0;
    sig.m_vMin = sig.m_vMean = sig.m_vMax = 0;
    updateSignature(i, sig, m_ranges[i]);
}
