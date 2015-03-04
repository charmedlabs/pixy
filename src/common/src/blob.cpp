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

#include <new>
#ifdef PIXY
#include "pixy_init.h"
#else
#include "pixymon.h"
#endif
#include "debug.h"
#include <blob.h>

#ifdef DEBUG
#ifndef HOST
#include <textdisp.h>
#else 
#include <stdio.h>
#endif

#define DBG_BLOB(x) x
#else
#define DBG_BLOB(x) 
#endif

bool CBlob::recordSegments= false;
// Set to true for testing code only.  Very slow!
bool CBlob::testMoments= false;
// Skip major/minor axis computation when this is false
bool SMoments::computeAxes= false;
int CBlob::leakcheck=0;

#ifdef INCLUDE_STATS
void SMoments::GetStats(SMomentStats &stats) const {
    stats.area= area;
    stats.centroidX = (float)sumX / (float)area;
    stats.centroidY = (float)sumY / (float)area;

    if (computeAxes) {
        // Find the eigenvalues and eigenvectors for the 2x2 covariance matrix:
        //
        // | sum((x-|x|)^2)        sum((x-|x|)*(y-|y|)) |
        // | sum((x-|x|)*(y-|y|))  sum((y-|y|)^2)       |

        // Values= 0.5 * ((sumXX+sumYY) +- sqrt((sumXX+sumYY)^2-4(sumXXsumYY-sumXY^2)))
        // .5 * (xx+yy) +- sqrt(xx^2+2xxyy+yy^2-4xxyy+4xy^2)
        // .5 * (xx+yy) +- sqrt(xx^2-2xxyy+yy^2 + 4xy^2)

        // sum((x-|x|)^2) =
        // sum(x^2) - 2sum(x|x|) + sum(|x|^2) =
        // sum(x^2) - 2|x|sum(x) + n|x|^2 =
        // sumXX - 2*centroidX*sumX + centroidX*sumX =
        // sumXX - centroidX*sumX

        // sum((x-|x|)*(y-|y|))=
        // sum(xy) - sum(x|y|) - sum(y|x|) + sum(|x||y|) =
        // sum(xy) - |y|sum(x) - |x|sum(y) + n|x||y| =
        // sumXY - centroidY*sumX - centroidX*sumY + sumX * centroidY =
        // sumXY - centroidX*sumY

        float xx= sumXX - stats.centroidX*sumX;
        float xyTimes2= 2*(sumXY - stats.centroidX*sumY);
        float yy= sumYY - stats.centroidY*sumY;
        float xxMinusyy = xx-yy;
        float xxPlusyy = xx+yy;
        float sq = sqrt(xxMinusyy * xxMinusyy + xyTimes2*xyTimes2);
        float eigMaxTimes2= xxPlusyy+sq;
        float eigMinTimes2= xxPlusyy-sq;
        stats.angle= 0.5*atan2(xyTimes2, xxMinusyy);
        //float aspect= sqrt(eigMin/eigMax);
        //stats.majorDiameter= sqrt(area/aspect);
        //stats.minorDiameter= sqrt(area*aspect);
        //
        // sqrt(eigenvalue/area) is the standard deviation
        // Draw the ellipse with radius of twice the standard deviation,
        // which is a diameter of 4 times, which is 16x inside the sqrt

        stats.majorDiameter= sqrt(8.0*eigMaxTimes2/area);
        stats.minorDiameter= sqrt(8.0*eigMinTimes2/area);
    }
}

void SSegment::GetMomentsTest(SMoments &moments) const {
    moments.Reset();
    int y= row;
    for (int x= startCol; x <= endCol; x++) {
        moments.area++;
        moments.sumX += x;
        moments.sumY += y;
        if (SMoments::computeAxes) {
            moments.sumXY += x*y;
            moments.sumXX += x*x;
            moments.sumYY += y*y;
        }
    }
}
#endif

///////////////////////////////////////////////////////////////////////////
// CBlob
CBlob::CBlob() 
{
    DBG_BLOB(leakcheck++);
    // Setup pointers
    firstSegment= NULL;
    lastSegmentPtr= &firstSegment;

    // Reset blob data
    Reset();
}

CBlob::~CBlob() 
{
    DBG_BLOB(leakcheck--);
    // Free segments, if any
    Reset();
}

void 
CBlob::Reset() 
{
    // Clear blob data
    moments.Reset();

    // Empty bounds
    right = -1;
    left = top = 0x7fff;
    lastBottom.row = lastBottom.invalid_row;
    nextBottom.row = nextBottom.invalid_row;

    // Delete segments if any
    SLinkedSegment *tmp;
    while(firstSegment!=NULL) {
        tmp = firstSegment;
        firstSegment = tmp->next;
        delete tmp;
    }
    lastSegmentPtr= &firstSegment;
}

void 
CBlob::NewRow() 
{
    if (nextBottom.row != nextBottom.invalid_row) {
        lastBottom= nextBottom;
        nextBottom.row= nextBottom.invalid_row;
    }
}

void 
CBlob::Add(const SSegment &segment) 
{
    // Enlarge bounding box if necessary
    UpdateBoundingBox(segment.startCol, segment.row, segment.endCol);

    // Update next attachment "surface" at bottom of blob
    if (nextBottom.row == nextBottom.invalid_row) {
        // New row.
        nextBottom= segment;
    } else {
        // Same row.  Add to right side of nextBottom.
        nextBottom.endCol= segment.endCol;
    }
    
    SMoments segmentMoments;
    segment.GetMoments(segmentMoments);
    moments.Add(segmentMoments);

    if (testMoments) {
#ifdef INCLUDE_STATS
        SMoments test;
        segment.GetMomentsTest(test);
        assert(test == segmentMoments);
#endif
    }
    if (recordSegments) {
        // Add segment to the _end_ of the linked list
        *lastSegmentPtr= new (std::nothrow) SLinkedSegment(segment);
        if (*lastSegmentPtr==NULL)
            return;
        lastSegmentPtr= &((*lastSegmentPtr)->next);
    }
}

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
void 
CBlob::Assimilate(CBlob &futileResister) 
{
    moments.Add(futileResister.moments);
    UpdateBoundingBox(futileResister.left,
                      futileResister.top,
                      futileResister.right);
    // Update lastBottom
    if (futileResister.lastBottom.endCol > lastBottom.endCol) {
        lastBottom.endCol= futileResister.lastBottom.endCol;
    }
    
    if (recordSegments) {
        // Take segments from futileResister, append on end
        *lastSegmentPtr= futileResister.firstSegment;
        lastSegmentPtr= futileResister.lastSegmentPtr;
        futileResister.firstSegment= NULL;
        futileResister.lastSegmentPtr= &futileResister.firstSegment;
        // Futile resister is left with no segments
    }
}

// Only updates left, top, and right.  bottom is updated 
// by UpdateAttachmentSurface below
void 
CBlob::UpdateBoundingBox(int newLeft, int newTop, int newRight) 
{
    if (newLeft  < left ) left = newLeft;
    if (newTop   < top  ) top  = newTop;
    if (newRight > right) right= newRight;
}

///////////////////////////////////////////////////////////////////////////
// CBlobAssembler

CBlobAssembler::CBlobAssembler() 
{
    activeBlobs= currentBlob= finishedBlobs= NULL;
    previousBlobPtr= &activeBlobs;
    currentRow=-1;
    maxRowDelta=1;
    m_blobCount=0;
}

CBlobAssembler::~CBlobAssembler() 
{
    // Flush any active blobs into finished blobs
    EndFrame();
    // Free any finished blobs
    Reset();
}

// Call once for each segment in the color channel
int CBlobAssembler::Add(const SSegment &segment) {
    if (segment.row != currentRow) {
        // Start new row
        currentRow= segment.row;
        RewindCurrent();
    }
    
    // Try to link this to a previous blob
    while (currentBlob) {
        if (segment.startCol > currentBlob->lastBottom.endCol) {
            // Doesn't connect.  Keep searching more blobs to the right.
            AdvanceCurrent();
        } else {
            if (segment.endCol < currentBlob->lastBottom.startCol) {
                // Doesn't connect to any blob.  Stop searching.
                break;
            } else {
                // Found a blob to connect to
                currentBlob->Add(segment);
                // Check to see if we attach to multiple blobs
                while(currentBlob->next &&
                      segment.endCol >= currentBlob->next->lastBottom.startCol) {
                    // Can merge the current blob with the next one,
                    // assimilate the next one and delete it.

                    // Uncomment this for verbose output for testing
                    // cout << "Merging blobs:" << endl
                    //     << " curr: bottom=" << currentBlob->bottom
                    //     << ", " << currentBlob->lastBottom.startCol
                    //     << " to " << currentBlob->lastBottom.endCol
                    //     << ", area " << currentBlob->moments.area << endl
                    //     << " next: bottom=" << currentBlob->next->bottom
                    //     << ", " << currentBlob->next->lastBottom.startCol
                    //     << " to " << currentBlob->next->lastBottom.endCol
                    //     << ", area " << currentBlob->next->moments.area << endl;

                    CBlob *futileResister = currentBlob->next;
                    // Cut it out of the list
                    currentBlob->next = futileResister->next;
                    // Assimilate it's segments and moments
                    currentBlob->Assimilate(*(futileResister));

                    // Uncomment this for verbose output for testing
                    // cout << " NEW curr: bottom=" << currentBlob->bottom
                    //     << ", " << currentBlob->lastBottom.startCol
                    //     << " to " << currentBlob->lastBottom.endCol
                    //     << ", area " << currentBlob->moments.area << endl;

                    // Delete it
                    delete futileResister;

                    BlobNewRow(&currentBlob->next);
                }
                return 0;
            }
        }
    }
    
    // Could not attach to previous blob, insert new one before currentBlob
    CBlob *newBlob= new (std::nothrow) CBlob();
    if (newBlob==NULL)
    {
        DBG("blobs %d\nheap full", m_blobCount);
        return -1;
    }
    m_blobCount++;
    newBlob->next= currentBlob;
    *previousBlobPtr= newBlob;
    previousBlobPtr= &newBlob->next;
    newBlob->Add(segment);
    return 0;
}

// Call at end of frame
// Moves all active blobs to finished list
void CBlobAssembler::EndFrame() {
    while (activeBlobs) {
        activeBlobs->NewRow();
        CBlob *tmp= activeBlobs->next;
        activeBlobs->next= finishedBlobs;
        finishedBlobs= activeBlobs;
        activeBlobs= tmp;
    }
}

int CBlobAssembler::ListLength(const CBlob *b) {
    int len= 0;
    while (b) {
        len++;
        b=b->next;
    }
    return len;
}


// Split a list of blobs into two halves
void CBlobAssembler::SplitList(CBlob *all,
                               CBlob *&firstHalf, CBlob *&secondHalf) {
    firstHalf= secondHalf= all;
    CBlob *ptr= all, **nextptr= &secondHalf;
    while (1) {
        if (!ptr->next) break;
        ptr= ptr->next;
        nextptr= &(*nextptr)->next;
        if (!ptr->next) break;
        ptr= ptr->next;
    }
    secondHalf= *nextptr;
    *nextptr= NULL;
}

// Merge maxelts elements from old1 and old2 into newptr
void CBlobAssembler::MergeLists(CBlob *&old1, CBlob *&old2,
                                CBlob **&newptr, int maxelts) {
    int n1= maxelts, n2= maxelts;
    while (1) {
        if (n1 && old1) {
            if (n2 && old2 && old2->moments.area > old1->moments.area) {
                // Choose old2
                *newptr= old2;
                newptr= &(*newptr)->next;
                old2= *newptr;
                --n2;
            } else {
                // Choose old1
                *newptr= old1;
                newptr= &(*newptr)->next;
                old1= *newptr;
                --n1;
            }
        }
        else if (n2 && old2) {
            // Choose old2
            *newptr= old2;
            newptr= &(*newptr)->next;
            old2= *newptr;
            --n2;
        } else {
            // Done
            return;
        }
    }
}

#ifdef DEBUG
void len_error() {
    printf("len error, wedging!\n");
    while(1);
}
#endif

// Sorts finishedBlobs in order of descending area using an in-place
// merge sort (time n log n)
void CBlobAssembler::SortFinished() {
    // Divide finishedBlobs into two lists
    CBlob *old1, *old2;

    if(finishedBlobs == NULL) {
        return;
    }

    DBG_BLOB(int initial_len= ListLength(finishedBlobs));
    DBG_BLOB(printf("BSort: Start 0x%x, len=%d\n", finishedBlobs,
               initial_len));
    SplitList(finishedBlobs, old1, old2);

    // First merge lists of length 1 into sorted lists of length 2
    // Next, merge sorted lists of length 2 into sorted lists of length 4
    // And so on.  Terminate when only one merge is performed, which
    // means we're completely sorted.
    
    for (int blocksize= 1; old2; blocksize <<= 1) {
        CBlob *new1=NULL, *new2=NULL, **newptr1= &new1, **newptr2= &new2;
        while (old1 || old2) {
            DBG_BLOB(printf("BSort: o1 0x%x, o2 0x%x, bs=%d\n",
                       old1, old2, blocksize));
            DBG_BLOB(printf("       n1 0x%x, n2 0x%x\n",
                       new1, new2));
            MergeLists(old1, old2, newptr1, blocksize);
            MergeLists(old1, old2, newptr2, blocksize);
        }
        *newptr1= *newptr2= NULL; // Terminate lists
        old1= new1;
        old2= new2;
    }
    finishedBlobs= old1;
    DBG_BLOB(AssertFinishedSorted());
    DBG_BLOB(int final_len= ListLength(finishedBlobs));
    DBG_BLOB(printf("BSort: DONE  0x%x, len=%d\n", finishedBlobs,
               ListLength(finishedBlobs)));
    DBG_BLOB(if (final_len != initial_len) len_error());
}

// Assert that finishedBlobs is in fact sorted.  For testing only.
void CBlobAssembler::AssertFinishedSorted() {
    if (!finishedBlobs) return;
    CBlob *i= finishedBlobs;
    CBlob *j= i->next;
    while (j) {
        assert(i->moments.area >= j->moments.area);
        i= j;
        j= i->next;
    }
}

void CBlobAssembler::Reset() {
    assert(!activeBlobs);
    currentBlob= NULL;
    currentRow=-1;
    m_blobCount=0;
    while (finishedBlobs) {
        CBlob *tmp= finishedBlobs->next;
        delete finishedBlobs;
        finishedBlobs= tmp;
    }
    DBG_BLOB(printf("after CBlobAssember::Reset, leakcheck=%d\n", CBlob::leakcheck));
}

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

void 
CBlobAssembler::BlobNewRow(CBlob **ptr) 
{
    short left, top, right, bottom;

    while (*ptr) {
        CBlob *blob= *ptr;
        blob->NewRow();
        if (currentRow - blob->lastBottom.row > maxRowDelta) {
            // Too many rows have elapsed.  Move it to the finished list
            *ptr= blob->next; // cut out of current list
            // check to see if it meets height and area constraints
            blob->getBBox(left, top, right, bottom);
            if (bottom-top>1) //&& blob->GetArea()>=MIN_COLOR_CODE_AREA)
            {
                // add to finished blobs
                blob->next= finishedBlobs;
                finishedBlobs= blob;
            }
            else
                delete blob;
        } else {
            // Blob is valid
            return;
        }
    }
}

void 
CBlobAssembler::RewindCurrent() 
{
    BlobNewRow(&activeBlobs);
    previousBlobPtr= &activeBlobs;
    currentBlob= *previousBlobPtr;

    if (currentBlob) BlobNewRow(&currentBlob->next);
}

void 
CBlobAssembler::AdvanceCurrent() 
{
    previousBlobPtr= &(currentBlob->next);
    currentBlob= *previousBlobPtr;
    if (currentBlob) BlobNewRow(&currentBlob->next);
}


