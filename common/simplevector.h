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

#ifndef SIMPLEVECTOR_H
#define SIMPLEVECTOR_H

#include <new>

#define SPARE_CAPACITY   16

template <typename Object> class SimpleVector
{
public:

    SimpleVector(int initSize = 0)
        : m_size(0), m_capacity(initSize + SPARE_CAPACITY)
    { m_objects = new Object[m_capacity]; }

    ~SimpleVector()
    { delete [] m_objects; }

    int resize(int newCapacity)
    {
        if(newCapacity < m_size)
            return 0;

        Object *oldArray = m_objects;

        m_objects = new (std::nothrow) Object[newCapacity];
        if (m_objects==NULL)
        {
            m_objects = oldArray;
            return -1;
        }
        for(int k = 0; k<m_size; k++)
            m_objects[k] = oldArray[k];

        m_capacity = newCapacity;

        delete [] oldArray;
        return 0;
    }

    Object & operator[](int index)
    { return m_objects[index]; }

    const Object& operator[](int index) const
    { return m_objects[index]; }

    bool empty() const
    { return size()==0; }

    int size() const
    { return m_size; }

    int capacity() const
    { return m_capacity; }

	const Object *data()
	{ return m_objects; }

    int push_back(const Object& x)
    {
        if(m_size == m_capacity)
            if (resize(m_capacity + SPARE_CAPACITY)<0)
                return -1;
        m_objects[m_size++] = x;
        return 0;
    }

    void pop_back()
    { m_size--; }

	void clear()
	{ m_size = 0; }

private:
    int m_size;
    int m_capacity;
    Object *m_objects;
};

#endif // SIMPLEVECTOR_H
