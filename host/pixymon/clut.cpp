#include <QDebug>
#include <stdio.h>
#include "clut.h"
#include "blobs.h"

// DEBUG
#include <QFile>

double e = 0.95;
double d = 1.5;
double d2 = 2.0;
double d3 = 2.0;
double minsat = 0.18;

static double** plotcluster(uint32_t* data, int d_len);
static void generatelut(double* L[], uint8_t* LUT);
static void plotpix(uint32_t* pixels, int pix_len, double* pp0, double* pp1, double* pp2);
static double tweakmean(double mean);
static double iterateline(double* P0, double* P1, int P_len, double* LI, double d, double e);
static int boundtest(double* P0, double* P1, int P_len, double* L, double dir);
static double mean(double* array, int dim, int len);
static double max(double* array, int len);
static void dot_2dim(double* array1[], double* array2[], double* ret, int len);
static double dot_1dim(double* array1, double* array2, int len);
static bool checkbounds(double* L[], int L_len, double c1, double c2);
static int32_t sign(int num);
static double sign(double num);

void CLUT::generateFromImgSample(uint32_t *data, int d_len, uint8_t* tempLut)
{
    double** L = plotcluster(data, d_len);
    generatelut(L, tempLut);

    delete(L[0]);
    delete(L[1]);
    delete(L);
}

static double** plotcluster(uint32_t* data, int d_len)
{
    double* ppp0 = new double[d_len/3];
    double* ppp1 = new double[d_len/3];
    double* ppp2 = new double[d_len/3];

    plotpix(data, d_len, ppp0, ppp1, ppp2);

    double meanx = mean(ppp1, 1, d_len/3);
    meanx = tweakmean(meanx);
    double meany = mean(ppp2, 2, d_len/3);
    meany = tweakmean(meany);
    double angle = atan2(meany, meanx);
    double slope = tan(angle);
    double uv[] = {cos(angle), sin(angle)};

    double* rmat[2];
    rmat[0] = new double[d_len];
    rmat[1] = new double[d_len];
    for (int i = 0; i < d_len; i++)
    {
        rmat[0][i] = uv[0];
        rmat[1][i] = uv[1];
    }

    double* pp1[2];
    pp1[0] = new double[d_len];
    pp1[1] = new double[d_len];
    for (int i = 0; i < d_len; i++)
    {
        pp1[0][i] = ppp1[i];
        pp1[1][i] = ppp2[i];
    }

    double* D = new double[d_len];
    dot_2dim(rmat, pp1, D, d_len);
    free(D);

    double anglep = angle + PI/2;
    double ps = tan(anglep);
    double lp[] = {ps, slope*meanx-ps*meanx};

    // Find upper and lower major lines
    double yu = iterateline(ppp1, ppp2, d_len/3, (double []){slope, 0}, fabs(0.001/cos(angle)), e);

    yu = yu + fabs(d * yu);
    double lu[] = {slope, yu};

    double yd = iterateline(ppp1, ppp2, d_len/3, (double []){slope, 0}, -1.0*fabs(0.001/cos(angle)), e);

    yd = yd - fabs(d*yd);
    double ld[] = {slope, yd};

    // Find inner and outer major lines
    // If uv[1] is negative, we want to iterate up (positive), so -sign(uv[1])
    double yl = iterateline(ppp1, ppp2, d_len/3, lp, -1.0*sign(uv[1])*fabs(0.001/cos(anglep)), e);
    yl = yl + -1*sign(uv[1])*fabs(d2*(yl-lp[1]));
    double xxl = yl / (slope-ps);
    double yyl = xxl * slope;
    double sat = dot_1dim(uv, (double []){xxl, yyl}, 2);
    if (sat < minsat)
    {
       double minl[] = {uv[0]*minsat, uv[1]*minsat};
       yl = minl[1] - ps*minl[0];
    }
    double ll[] = {ps, yl};

    // If uv[1] is negative, we want to iterate down (negative), so sign(uv[1])
    double yr = iterateline(ppp1, ppp2, d_len/3, lp, sign(uv[1])*fabs(0.001/cos(anglep)), e);
    yr = yr - -1*sign(uv[1])*fabs(d3*(yr-lp[1]));
    double lr[] = {ps, yr};

    free(pp1[0]);
    free(pp1[1]);

    free(ppp0);
    free(ppp1);
    free(ppp2);

    double** L = new double*[2];
    L[0] = new double[4];
    L[1] = new double[4];
    if (ll[1] > lr[1])
    {
        L[0][0] = lu[0];
        L[0][1] = ld[0];
        L[0][2] = ll[0];
        L[0][3] = lr[0];

        L[1][0] = lu[1];
        L[1][1] = ld[1];
        L[1][2] = ll[1];
        L[1][3] = lr[1];
    }
    else
    {
        L[0][0] = lu[0];
        L[0][1] = ld[0];
        L[0][2] = lr[0];
        L[0][3] = ll[0];

        L[1][0] = lu[1];
        L[1][1] = ld[1];
        L[1][2] = lr[1];
        L[1][3] = ll[1];
    }

    return L;
}

static void plotpix(uint32_t* pixels, int pix_len, double* pp0, double* pp1, double* pp2)
{
    int count = 0;
    for (int i = 0; i < pix_len; i+=3) {
        double r = (double)pixels[i];
        double g = (double)pixels[i+1];
        double b = (double)pixels[i+2];

        double L = (r+g+b)/(3.0*255.0);
        pp0[count] = L;
        pp1[count] = (r-g)/255.0;
        pp2[count] = (b-g)/255.0;

        count++;
    }
}

static double tweakmean(double mean)
{
    double ret = 0.0;
    if (fabs(mean) < 0.0001)
    {
        if (mean > 0)
            ret = 0.0001;
        else
            ret = -0.0001;
    }
    else
    {
        ret = mean;
    }
    return ret;
}

static double iterateline(double* P0, double* P1, int P_len, double* LI, double d, double e)
{
    double ret = LI[1];
    double d_sign = 1;
    if (d < 0)
        d_sign = -1;

    while (1)
    {
        double btest = boundtest(P0, P1, P_len, (double []) {LI[0], ret}, d_sign);
        double temp = (btest/double(P_len));

        if (temp >= e)
        {
            break;
        }
        ret = ret + d;
    }
    return ret;
}

static int boundtest(double* P0, double* P1, int P_len, double* L, double dir)
{
    int n = 0;
    for (int i = 0; i < P_len; i++)
    {
        double y = P0[i]*L[0] + L[1];

        if (dir > 0)
        {
            if (P1[i] < y)
                n++;
        }
        else
        {
            if (P1[i] > y)
                n++;
        }
    }
    return n;
}

static void generatelut(double* L[], uint8_t* LUT)
{
    int32_t C2[256];
    for (int32_t i = 0; i < 128; i++) {
        C2[i] = i;
    }
    for (int32_t i = -128; i < 0; i++) {
        C2[i+256] = i;
    }

    int index = 0;
    for (int32_t i = 0; i < 256; i++)
    {
        for (int32_t j = 0; j < 256; j++)
        {
            //double c1 = (double)((double)i / (double)127);
            //double c2 = (double)((double)j / (double)127);
            double c1 = (double)((double)C2[i] / (double)127);
            double c2 = (double)((double)C2[j] / (double)127);

            // DEBUG
            /*QFile file("pixiC.txt");
            file.open(QIODevice::WriteOnly | QIODevice::Append);
            QTextStream out(&file);

            out << c1;
            out << " ";
            out << c2;
            out << " (i=";
            out << C2[i];
            out << ", j=";
            out << C2[j];
            out << ")";
            out << "\n";*/

            if (checkbounds(L, 4, c1, c2))
            {
                LUT[index] = 1;
            }
            else
            {
                LUT[index] = 0;
            }
            index++;
        }
    }
}

static double mean(double* array, int dim, int len)
{
    double total = 0;
    for (int i = 0; i < len; i++)
    {
        total += array[i];
    }

    return total/(double)len;
}

static double max(double* array, int len)
{
    int ret = array[0];
    for (int i = 0; i < len; i++)
    {
        if (array[i] > ret)
            ret = array[i];
    }
    return ret;
}

static void dot_2dim(double* array1[], double* array2[], double* ret, int len)
{
    for (int i = 0; i < len; i++)
    {
        ret[i] += array1[0][i]*array2[0][i] + array1[1][i]*array2[1][i];
    }
}

static double dot_1dim(double* array1, double* array2, int len)
{
    double ret = 0.0;
    for (int i = 0; i < len; i++)
    {
        ret += array1[i]*array2[i];
    }
    return ret;
}

static bool checkbounds(double* L[], int L_len, double c1, double c2)
{
    bool ret = false;
    double *Y = new double[L_len];
    for (int i = 0; i < L_len; i++)
        Y[i] = L[0][i]*c1 + L[1][i];

    // DEBUG
    double y1 = Y[0];
    double y2 = Y[1];
    double y3 = Y[2];
    double y4 = Y[3];

    /*QFile file("pixiCheckbounds.txt");
    file.open(QIODevice::Append | QIODevice::WriteOnly);
    QTextStream out(&file);

    out << c2;
    out << " ";
    out << y1;
    out << " ";
    out << y2;
    out << " ";
    out << y3;
    out << " ";
    out << y4;
    out << "\n";*/

    if ((Y[0] > c2) && (Y[1] < c2) && (Y[2] > c2) && (Y[3] < c2))
    {
        ret = true;
    }

    free(Y);
    return ret;
}

static int32_t sign(int num)
{
    return abs(num)/num;
}

static double sign(double num)
{
    return fabs(num)/num;
}
