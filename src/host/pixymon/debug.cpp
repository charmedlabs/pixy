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

#include "debug.h"
#include "interpreter.h"
#include "parameters.h"

MON_MODULE(Debug);

uint g_debug = 0;

Debug::Debug(Interpreter *interpreter) : MonModule(interpreter)
{
    m_interpreter->m_pixymonParameters->add("Debug", PT_INT32, 0,
        "This parameter sets the debugging level, 0=no debug info, >0=debugging level (try 1)");
}


void Debug::paramChange()
{
    QVariant val;

    if (pixymonParameterChanged("Debug", &val))
        g_debug = val.toUInt();

}

