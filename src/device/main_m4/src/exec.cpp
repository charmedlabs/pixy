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
#include <string.h>
#include "pixy_init.h"
#include "misc.h"
#include "exec.h"
#include "button.h"
#include "camera.h"
#include "conncomp.h"
#include "serial.h"
#include "rcservo.h"
#include "progpt.h"
#include "progblobs.h"
#include "progchase.h"
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
	"@r always returns 0 and an array of 3 uint16 values: major, minor, and build versions"
	},
	{
	"versionType",
	(ProcPtr)exec_versionType, 
	{END}, 
	"Get firmware type"
	"@r always returns 0 and a null-terminated string that describes the type of firmware"
	},
	{
	"getAction",
	(ProcPtr)exec_getAction, 
	{CRP_UINT16, END}, 
	"Get the action scriptlet assocated with the index argument"
	"@p action index"
	"@r returns 0 if successful, -1 otherwise, or if index is out of range"
	},
	{
	"getUID",
	(ProcPtr)exec_getUID, 
	{END}, 
	"Get the unique ID of this Pixy"
	"@r returns 32-bit unique ID"
	},
	END
};

static const ActionScriptlet actions[]=
{
	{
	"Run pan/tilt demo", 
	"runprog 1\n"
	}, 
	{
	"Set signature 1...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 1\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 2...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 2\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 3...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 3\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 4...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 4\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 5...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 5\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 6...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 6\n"
	"runprogArg 8 1\n"
	},
	{
	"Set signature 7...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 0 7\n"
	"runprogArg 8 1\n"
	},

	{
	"Set CC signature 1...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 1\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 2...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 2\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 3...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 3\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 4...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 4\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 5...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 5\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 6...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 6\n"
	"runprogArg 8 1\n"
	},
	{
	"Set CC signature 7...", 
	"cam_getFrame 0x21 0 0 320 200\n"
    "cc_setSigRegion 1 7\n"
	"runprogArg 8 1\n"
	},

	{
	"Clear signature...", 
    "cc_clearSig\n"
	"run\n"
	},
	{
	"Clear all signatures", 
    "cc_clearAllSig\n"
	"run\n"
	},
	{
	"Restore default parameter values", 
    "prm_restore\n"
	"close\n"
	}
};

uint8_t g_running = false;
uint8_t g_run = false;
uint8_t g_program = 0;
uint8_t g_startupProgram = 0;
// this variable prevents a race condition between the program selection upon power up and any
// program selection by PixyMon when it connects
int8_t g_programChirp = -1;	 
int32_t g_execArgChirp = 0;
uint8_t g_override = 0;
int32_t g_execArg = 0;  // this arg mechanism is lame... should introduce an argv type mechanism 
uint8_t g_debug = 0;

static ChirpProc g_runM0 = -1;
static ChirpProc g_runningM0 = -1;
static ChirpProc g_stopM0 = -1;
static uint8_t g_progM0 = 0;
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
		g_progTable[EXEC_VIDEO_PROG] = prog;
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

int32_t exec_runprog(const uint8_t &progNum, Chirp *chirp)
{	
	uint8_t progNum2 = progNum;
			   
	if (chirp)
	{
		g_programChirp = progNum2;
		g_execArgChirp = 0;
	}
	if (g_programChirp>=0) // if PixyMon is setting program, it overrides
		progNum2 = g_programChirp;

	g_program = 0;
	if (progNum2>=EXEC_MAX_PROGS || g_progTable[progNum2]==NULL)
		return -1;

	if (g_programChirp>=0) // if PixyMon is setting program, save arg too
		g_execArg = g_execArgChirp;
	else
		g_execArg = 0;

	g_program = progNum2;
	return exec_run();
}

int32_t exec_runprogArg(const uint8_t &progNum, const int32_t &arg, Chirp *chirp)
{
	int32_t res = exec_runprog(progNum, chirp);
	if (res<0)
		return res;

	if (chirp)
		g_execArgChirp = arg;
	if (g_programChirp>=0) // if PixyMon is setting program, it overrides
		g_execArg = g_execArgChirp;
	else
		g_execArg = arg;
	return 0;
}

int32_t exec_list()
{
	int i;
	for (i=0; g_progTable[i]; i++)
		cprintf("%d: %s, %s\n", i, g_progTable[i]->progName, g_progTable[i]->desc);

 	return 0;
}

int32_t exec_version(Chirp *chirp)
{
	uint16_t ver[] = {FW_MAJOR_VER, FW_MINOR_VER, FW_BUILD_VER};

	//cprintf("Pixy firmware version %d.%d.%d\n", ver[0], ver[1], ver[2]);
	if (chirp)
		CRP_RETURN(chirp, UINTS16(sizeof(ver), ver), END);

	return 0;
}

int32_t exec_versionType(Chirp *chirp)
{
	if (chirp)
		CRP_RETURN(chirp, STRING(FW_TYPE), END);

	return 0;
}


int32_t exec_getAction(const uint16_t &index, Chirp *chirp)
{
	int n = sizeof(actions)/sizeof(ActionScriptlet);

	if (index>=n)
		return -1;

	if (chirp)
		CRP_RETURN(chirp, STRING(actions[index].action), STRING(actions[index].scriptlet), END);

	return 0;		
}

uint32_t exec_getUID()
{
	uint32_t val;
	volatile uint32_t *mem;

	for (val=0, mem=(volatile uint32_t *)0x40045000; mem<(volatile uint32_t *)0x40045010; mem++)
		val += *mem;

	return val;
}

int exec_runM0(uint8_t prog)
{
	int responseInt;

	g_chirpM0->callSync(g_runM0, UINT8(prog), END_OUT_ARGS,
		&responseInt, END_IN_ARGS);

	g_progM0 = prog;

	return responseInt;
}

int exec_stopM0()
{
	int responseInt;

	g_chirpM0->callSync(g_stopM0, END_OUT_ARGS,
		&responseInt, END_IN_ARGS);

	return responseInt;
}

uint8_t exec_runningM0()
{
	uint32_t responseInt;

	g_chirpM0->callSync(g_runningM0, END_OUT_ARGS,
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
	uint8_t progs;


	prm_get("Startup program", &g_startupProgram, END);
	g_program = g_startupProgram;

	// count number of progs
	for (progs=0; g_progTable[progs]; progs++);

	// select using button state machine
	g_bMachine->selectProgram(progs, &g_program);

	exec_runprog(g_program);
}

static void loadParams()
{
#ifndef LEGO
	int i;
	char buf[256], buf2[64];

	// create program menu
	strcpy(buf, "Selects the program number that's run upon power-up. @c Expert");
	for (i=0; g_progTable[i]; i++)
	{
		sprintf(buf2, " @s %d=%s", i, g_progTable[i]->progName);
		strcat(buf, buf2);
	} 

	// exec's params added here
	prm_add("Startup program", 0, buf, UINT8(0), END);
#endif
	prm_add("Debug", 0, 
		"@c Expert Sets the debug level for the firmware. (default 0)", UINT8(0), END);
	
	prm_get("Debug", &g_debug, END);
}

void exec_loadParams()
{
 	cc_loadParams();
	ser_loadParams();
	cam_loadParams();
#ifndef LEGO
	rcs_loadParams();

	ptLoadParams();
	//chaseLoadParams();
#endif
	loadParams(); // local
}


void exec_loop()
{
	uint8_t state = 0;
	bool prevConnected = false;
	bool connected;

#ifdef LEGO
	exec_runprog(0);
#else
	exec_select();
#endif

	while(1)
	{
		connected = g_chirpUsb->connected();

		exec_periodic();

		switch (state)
		{
		case 0:	// setup state
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
			else if (prevConnected && !connected) // if we disconnect from pixymon, revert back to default program
			{
				prm_resetShadows(); // shadows are no longer valid now that the host is disconnected.
				g_programChirp = -1; // reset this value to "unset"
				exec_runprog(g_startupProgram); // run default program
				state = 0; // setup state
			}
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
			else if (!connected || !USB_Configuration) // if we disconnect from pixy or unplug cable, revert back to default program
			{
				g_programChirp = -1; // reset this value to "unset"
				exec_runprog(g_startupProgram); // run default program
				state = 0;	// back to setup state
			}
			break;

		default:
			state = 3; // stop state				
		}

		prevConnected = connected;
	}
}

uint8_t exec_pause()
{
	uint8_t running;
  	running = exec_runningM0();
	if (running)
		exec_stopM0();
	return running;
}

void exec_resume()
{
	g_qqueue->flush();
	exec_runM0(g_progM0);
}

void exec_sendEvent(Chirp *chirp, uint32_t event)
{
	if (chirp)
		CRP_SEND_XDATA(chirp, HTYPE(FOURCC('E','V','T','1')), INT32(event));
}

