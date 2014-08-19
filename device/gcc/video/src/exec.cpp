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
#include "pixy_init.h"
#include "misc.h"
#include "exec.h"
#include "button.h"
#include "camera.h"
#include "led.h"
#include "conncomp.h"
#include "serial.h"
#include "rcservo.h"
#include "progpt.h"
#include "param.h"

static const ProcModule g_module[] =
{
	{
	"running",
	(ProcPtr)exec_running, 
	{END}, 
	"Is a program running?"
	"@r 1 if a program is running, 2 if running in \"forced\" state, 0 if not"
	},
	{
	"stop",
	(ProcPtr)exec_stop, 
	{END}, 
	"Stop execution of the current program"
	"@r always returns 0"
	},
	{
	"run",
	(ProcPtr)exec_run, 
	{END}, 
	"Run the current program"
	"@r returns 0 if successful, -1 otherwise"
	},
	{
	"runprog",
	(ProcPtr)exec_runprog, 
	{CRP_UINT8, END}, 
	"Run the specified program"
	"@p program number"
	"@r returns 0 if successful, -1 otherwise"
	},
	{
	"runprogArg",
	(ProcPtr)exec_runprogArg, 
	{CRP_UINT8, CRP_UINT32, END}, 
	"Run the specified program with an argument"
	"@p program number"
	"@p argument to be passed to program"
	"@r returns 0 if successful, -1 otherwise"
	},
	{
	"progs",
	(ProcPtr)exec_list, 
	{END}, 
	"List available programs"
	"@r always returns 0"
	},
	{
	"version",
	(ProcPtr)exec_version, 
	{END}, 
	"Get firmware version"
	"@r always returns 0 and an array of 3 uint16 values: major, minor, and build versions."
	},
	END
};

uint8_t g_running = false;
uint8_t g_run = false;
uint8_t g_program = 0;
uint8_t g_override = 0;
int32_t g_execArg = 0;  // this arg mechanism is lame... should introduce an argv type mechanism 

static ChirpProc g_runM0 = -1;
static ChirpProc g_runningM0 = -1;
static ChirpProc g_stopM0 = -1;
static Program *g_progTable[EXEC_MAX_PROGS];
static void loadParams();

ButtonMachine *g_bMachine = NULL;


int exec_init(Chirp *chirp)
{
	g_bMachine = new ButtonMachine;

	chirp->registerModule(g_module);

	g_runM0 = g_chirpM0->getProc("run", NULL);
	g_runningM0 = g_chirpM0->getProc("running", NULL);
	g_stopM0 = g_chirpM0->getProc("stop", NULL);	

	loadParams();		

	return 0;	
}


int exec_addProg(Program *prog, bool video)
{
	int i;

	if (video)
		g_progTable[EXEC_VIDEO_PROG-1] = prog;
	else
	{
		for (i=0; g_progTable[i]; i++)

		if (i>=EXEC_MAX_PROGS)
			return -1;

   		g_progTable[i] = prog;
	}
	return 0;
}


uint32_t exec_running()
{
 	if (g_running)
		return g_running;
	if (g_override)
		return 2; // we're not running and we're pressing the button
	return 0;
}

int32_t exec_stop()
{
	g_run = 0;
	return 0;
}

int32_t exec_run()
{
	g_run = true;
	g_running = true;		
	return 0;
}


int32_t exec_runprog(const uint8_t &progNum)
{			   
	if (progNum!=0 && (progNum>EXEC_MAX_PROGS || g_progTable[progNum-1]==NULL))
		return -1;

	g_execArg = 0;

	if (progNum==0) // default program!
		prm_get("Default program", &g_program, END);
  	else
		g_program = progNum-1;
	return exec_run();
}

int32_t exec_runprogArg(const uint8_t &progNum, const int32_t &arg)
{
	int32_t res = exec_runprog(progNum);
	if (res<0)
		return res;

	g_execArg = arg;
	return 0;
}

int32_t exec_list()
{
	int i;
	for (i=0; g_progTable[i]; i++)
		cprintf("%d: %s, %s\n", i+1, g_progTable[i]->progName, g_progTable[i]->desc);

 	return 0;
}

int32_t exec_version(Chirp *chirp)
{
	uint16_t ver[] = {FW_MAJOR_VER, FW_MINOR_VER, FW_BUILD_VER};

	cprintf("Pixy firmware version %d.%d.%d\n", ver[0], ver[1], ver[2]);
	if (chirp)
		CRP_RETURN(chirp, UINTS16(sizeof(ver), ver), END);

	return 0;
}

int exec_runM0(uint8_t prog)
{
	int responseInt;

	g_chirpM0->callSync(g_runM0, UINT8(prog), END_OUT_ARGS,
		&responseInt, END_IN_ARGS);

	return responseInt;
}

int exec_stopM0()
{
	int responseInt;

	g_chirpM0->callSync(g_stopM0, END_OUT_ARGS,
		&responseInt, END_IN_ARGS);

	return responseInt;
}

void exec_periodic()
{
	periodic();
	g_override = g_bMachine->handleSignature();
	if (prm_dirty())
		exec_loadParams();
}

void exec_select()
{
	uint8_t prog, progs;

	// count number of progs
	for (progs=0; g_progTable[progs]; progs++);

	// select using button state machine
	prog = g_bMachine->selectProgram(progs);

	// set it up to run
	exec_runprog(prog);
}

static void loadParams()
{
	// exec's params added here
	prm_add("Default program", 0, 
		"Selects the program number that's run by default upon power-up. (default 0)", UINT8(0), END);
}

void exec_loadParams()
{
 	cc_loadParams();
	ser_loadParams();
	cam_loadParams();
	rcs_loadParams();

	ptLoadParams();

	loadParams(); // local
}


void exec_loop()
{
	uint8_t state = 0;
	bool prevConnected = false;
	bool connected;

	exec_select();

	g_program = 7;

	while(1)
	{
		connected = g_chirpUsb->connected();

		exec_periodic();

		switch (state)
		{
		case 0:	// setup state
			led_set(0);  // turn off any stray led
			if ((*g_progTable[g_program]->setup)()<0)
				state = 3; // stop state
			else 
				state = 1; // loop state
			break;

		case 1:	 // loop state
			if (g_override)
			{
				// need to stop M0 because it's using the same memory and can possibly interfere.
				// For example if we try to grab a raw frame while M0 is running (gathering RLS values)
				// M0 could overwrite the frame memory with RLS scratch data.
				exec_stopM0(); 
				state = 2; // override state
			}
			else if (!g_run  || (*g_progTable[g_program]->loop)()<0)
				state = 3; // stop state
#if 0 // ***keil
			else if (prevConnected && !connected) // if we disconnect from pixymon, revert back to default program
			{
				exec_runprog(0); // run default program
				state = 0; // setup state
			}
#endif
			break;

		case 2:	// override state
			if (!g_override) 
				state = 0; // back to setup state
			break;

		case 3:	// stop state
			// set variable to indicate we've stopped
			g_run = false;
			g_running = false;
			// stop M0
			exec_stopM0();
			state = 4; // wait for run state
			break;

		case 4:	// wait for run state
			if (g_run) 
			{
				exec_run();
				state = 0; // back to setup state
			}
#if 0 // ***keil
			else if (!connected || !USB_Configuration) // if we disconnect from pixy or unplug cable, revert back to default program
			{
				exec_runprog(0); // run default program
				state = 0;	// back to setup state
			}
#endif
			break;

		default:
			state = 3; // stop state				
		}

		prevConnected = connected;
	}
}
