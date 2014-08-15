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
#ifndef _BLOB_H
#define _BLOB_H

// TODO
//
// *** Priority 1
//
// *** Priority 2:
//
// *** Priority 3:
//
// *** Priority 4:
//
// Think about heap management of CBlobs
// Think about heap management of SLinkedSegments
//
// *** Priority 5 (maybe never do):
// 
// Try small and large SMoments structure (small for segment)
// Try more efficient SSegment structure for lastBottom, nextBottom
//
// *** DONE
//
// DONE Compute elongation, major/minor axes (SMoments::GetStats)
// DONE Make XRC LUT
// DONE Use XRC LUT
// DONE Optimize blob assy
// DONE Start compiling
// DONE Conditionally record segments
// DONE Ask rich about FP, trig
// Take segmented image in (DONE in imageserver.cc, ARW 10/7/04)
// Produce colored segmented image out (DONE in imageserver.cc, ARW 10/7/04)
// Draw blob stats in image out (DONE for centroid, bounding box
//                               in imageserver.cc, ARW 10/7/04)
// Delete segments when deleting blob (DONE, ARW 10/7/04)
// Check to see if we attach to multiple blobs  (DONE, ARW 10/7/04)
// Sort blobs according to area  (DONE, ARW 10/7/04)
// DONE Sort blobs according to area
// DONE Clean up code

#include <stdlib.h>
#include <assert.h>
//#include <memory.h>
#include <math.h>

//#define INCLUDE_STATS

// Uncomment this for verbose output for testing
//#include <iostream.h>

struct SMomentStats {
    int   area;
    // X is 0 on the left side of the image and increases to the right
    // Y is 0 on the top of the image and increases to the bottom
    float centroidX, centroidY;
    // angle is 0 to PI, in radians.
    // 0 points to the right (positive X)
    // PI/2 points downward (positive Y)
    float angle;
    float majorDiameter;
    float minorDiameter;
};

// Image size is 352x278
// Full-screen blob area is 97856
// Full-screen centroid is 176,139
// sumX, sumY is then 17222656, 13601984; well within 32 bits
struct SMoments {
    // Skip major/minor axis computation when this is false
    static bool computeAxes;

    int area; // number of pixels
    void Reset() {
        area = 0;
#ifdef INCLUDE_STATS
        sumX= sumY= sumXX= sumYY= sumXY= 0;
#endif
    }
#ifdef INCLUDE_STATS
    int sumX; // sum of pixel x coords
    int sumY; // sum of pixel y coords
    // XX, XY, YY used for major/minor axis calculation
    long long sumXX; // sum of x^2 for each pixel
    long long sumYY; // sum of y^2 for each pixel
    long long sumXY; // sum of x*y for each pixel
#endif
    void Add(const SMoments &moments) {
        area += moments.area;
#ifdef INCLUDE_STATS
        sumX += moments.sumX;
        sumY += moments.sumY;
        if (computeAxes) {
            sumXX += moments.sumXX;
            sumYY += moments.sumYY;
            sumXY += moments.sumXY;
        }
#endif
    }
#ifdef INCLUDE_STATS
    void GetStats(SMomentStats &stats) const;
    bool operator==(const SMoments &rhs) const {
        if (area != rhs.area) return 0;
        if (sumX != rhs.sumX) return 0;
        if (sumY != rhs.sumY) return 0;
        if (computeAxes) {
            if (sumXX != rhs.sumXX) return 0;
            if (sumYY != rhs.sumYY) return 0;
            if (sumXY != rhs.sumXY) return 0;
        }
        return 1;
    }
#endif
};

struct SSegment {
    unsigned char  model    : 3 ; // which color channel
    unsigned short row      : 9 ;
    unsigned short startCol : 10; // inclusive
    unsigned short endCol   : 10; // inclusive

    const static short invalid_row= 0x1ff;

    // Sum 0^2 + 1^2 + 2^2 + ... + n^2 is (2n^3 + 3n^2 + n) / 6
    // Sum (a+1)^2 + (a+2)^2 ... b^2 is (2(b^3-a^3) + 3(b^2-a^2) + (b-a)) / 6
    //
    // Sum 0+1+2+3+...+n is (n^2 + n)/2
    // Sum (a+1) + (a+2) ... b is (b^2-a^2 + b-a)/2

    void GetMoments(SMoments &moments) const {
        int s= startCol - 1;
        int e= endCol;

        moments.area  = (e-s);
#ifdef INCLUDE_STATS
        int e2= e*e;
        int y= row;
        int s2= s*s;
        moments.sumX = ( (e2-s2) + (e-s) ) / 2;
        moments.sumY = (e-s) * y;

        if (SMoments::computeAxes) {
            int e3= e2*e;
            int s3= s2*s;
            moments.sumXY= moments.sumX*y;
            moments.sumXX= (2*(e3-s3) + 3*(e2-s2) + (e-s)) / 6;
            moments.sumYY= moments.sumY*y;
        }
#endif
    }
#ifdef INCLUDE_STATS
    void GetMomentsTest(SMoments &moments) const;
#endif
};

struct SLinkedSegment {
    SSegment segment;
    SLinkedSegment *next;
    SLinkedSegment(const SSegment &segmentInit) :
        segment(segmentInit), next(NULL) {}
};

class CBlob {
    // These are at the beginning for fast inclusion checking
public:
    static int leakcheck;
    CBlob *next;            // next ptr for linked list

    // Bottom of blob, which is the surface we'll attach more segments to
    // If bottom of blob contains multiple segments, this is the smallest
    // segment containing the multiple segments
    SSegment lastBottom;

    // Next bottom of blob, currently under construction
    SSegment nextBottom;

    // Bounding box, inclusive.  nextBottom.row contains the "bottom"
    short left, top, right;

    void getBBox(short &leftRet, short &topRet,
                 short &rightRet, short &bottomRet) {
        leftRet= left;
        topRet= top;
        rightRet= right;
        bottomRet= lastBottom.row;
    }

    // Segments which compose the blob
    // Only recorded if CBlob::recordSegments is true
    // firstSegment points to first segment in linked list
    SLinkedSegment *firstSegment;
    // lastSegmentPtr points to the next pointer field _inside_ the
    // last element of the linked list.  This is the field you would
    // modify in order to append to the end of the list.  Therefore
    // **lastSegmentPtr should always equal to NULL.
    // When the list is empty, lastSegmentPtr actually doesn't point inside
    // a SLinkedSegment structure at all but instead at the firstSegment
    // field above, which in turn is NULL.
    SLinkedSegment **lastSegmentPtr;

    SMoments moments;

    static bool recordSegments;
    // Set to true for testing code only.  Very slow!
    static bool testMoments;

    CBlob();
    ~CBlob();

    int GetArea() const {
        return(moments.area);
    }

    // Clear blob data and free segments, if any
    void Reset();
    
    void NewRow();

    void Add(const SSegment &segment);

    // This takes futileResister and assimilates it into this blob
    //
    // Takes advantage of the fact that we are always assembling top to
    // bottom, left to right.
    //
    // Be sure to call like so:
    // leftblob.Assimilate(rightblob);
    //
    // This lets us assume two things:
    // 1) The assimilated blob contains no segments on the current row
    // 2) The assimilated blob lastBottom surface is to the right
    //    of this blob's lastBottom surface
    void Assimilate(CBlob &futileResister);

    // Only updates left, top, and right.  bottom is updated
    // by UpdateAttachmentSurface below
    void UpdateBoundingBox(int newLeft, int newTop, int newRight);
};

// Strategy for using CBlobAssembler:
//
// Make one CBlobAssembler for each color channel.
// CBlobAssembler ignores the model index, so you need to be sure to
// only pass the correct segments to each CBlobAssembler.
//
// At the beginning of a frame, call Reset() on each assembler
// As segments appear, call Add(segment)
// At the end of a frame, call EndFrame() on each assembler
// Get blobs from finishedBlobs.  Blobs will remain valid until
//    the next call to Reset(), at which point they will be deleted.
//
// To get statistics for a blob, do the following:
//  SMomentStats stats;
//  blob->moments.GetStats(stats);
// (See imageserver.cc: draw_blob() for an example)

class CBlobAssembler {
    short currentRow;

    // Active blobs, in left to right order
    // (Active means we are still potentially adding segments)
    CBlob *activeBlobs;

    // Current candidate for adding a segment to.  This is a member
    // of activeBlobs, and scans left to right as we search the active blobs.
    CBlob *currentBlob;

    // Pointer to pointer to current candidate, which is actually the pointer
    // to the "next" field inside the previous candidate, or a pointer to
    // the activeBlobs field of this object if the current candidate is the
    // first element of the activeBlobs list.  Used for inserting and
    // deleting blobs.
    CBlob **previousBlobPtr;

public:
    // Blobs we're no longer adding to
    CBlob *finishedBlobs;
    short maxRowDelta;
    static bool keepFinishedSorted;

public:
    CBlobAssembler();
    ~CBlobAssembler();

    // Call prior to starting a frame
    // Deletes any previously created blobs
    void Reset();


    // Call once for each segment in the color channel
    int Add(const SSegment &segment);

    // Call at end of frame
    // Moves all active blobs to finished list
    void EndFrame();

    int ListLength(const CBlob *b);
    
    // Split a list of blobs into two halves
    void SplitList(CBlob *all, CBlob *&firstHalf, CBlob *&secondHalf);

    // Merge maxelts elements from old1 and old2 into newptr
    void MergeLists(CBlob *&old1, CBlob *&old2, CBlob **&newptr, int maxelts);

    // Sorts finishedBlobs in order of descending area using an in-place
    // merge sort (time n log n)
    void SortFinished();

    // Assert that finishedBlobs is in fact sorted.  For testing only.
    void AssertFinishedSorted();

protected:
    // Manage currentBlob
    //
    // We always want to guarantee that both currentBlob
    // and currentBlob->next have had NewRow() called, and have
    // been validated to remain on the active list.  We could just
    // do this for all activeBlobs at the beginning of each row,
    // but it's less work to only do it on demand as segments come in
    // since it might allow us to skip blobs for a given row
    // if there are no segments which might overlap.

    // BlobNewRow:
    //
    // Tell blob there is a new row of data, and confirm that the
    // blob should still be on the active list by seeing if too many
    // rows have elapsed since the last segment was added.
    //
    // If blob should no longer be on the active list, remove it and
    // place on the finished list, and skip to the next blob.
    //
    // Call this either zero or one time per blob per row, never more.
    //
    // Pass in the pointer to the "next" field pointing to the blob, so
    // we can delete the blob from the linked list if it's not valid.

    void BlobNewRow(CBlob **ptr);
    void RewindCurrent();
    void AdvanceCurrent();

    int m_blobCount;
};

#endif // _BLOB_H
