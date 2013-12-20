#include <stdio.h>
#include "pixy_init.h"
#include "misc.h"
#include "exec.h"
#include "camera.h"
#include "qqueue.h"

static const ProcModule g_module[] =
{
	{
	"running",
	(ProcPtr)exec_running, 
	{END}, 
	"Is a program running?"
	"@r 1 if a program is running, 0 if not"
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
	"progs",
	(ProcPtr)exec_list, 
	{END}, 
	"List available programs"
	"@r always returns 0"
	},
	END
};

uint8_t g_running = false;
uint8_t g_run = false;
uint8_t g_program = 0;

static ChirpProc g_runM0 = -1;
static ChirpProc g_runningM0 = -1;
static ChirpProc g_stopM0 = -1;
static Program *g_progTable[EXEC_MAX_PROGS];

extern Program g_progVideo;
extern Program g_progBlobs;

int exec_init(Chirp *chirp)
{
	chirp->registerModule(g_module);

	g_runM0 = g_chirpM0->getProc("run", NULL);
	g_runningM0 = g_chirpM0->getProc("running", NULL);
	g_stopM0 = g_chirpM0->getProc("stop", NULL);	
		
	exec_addProg(&g_progVideo);
	exec_addProg(&g_progBlobs);
	exec_runprog(0);

	return 0;	
}

int exec_addProg(Program *prog)
{
	int i;

	for (i=0; g_progTable[i]; i++)

	if (i>=EXEC_MAX_PROGS)
		return -1;

   g_progTable[i] = prog;
   return 0;
}

uint32_t exec_running()
{
	return (uint32_t)g_running;
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
	if (progNum>=EXEC_MAX_PROGS || g_progTable[progNum]==NULL)
		return -1;

	g_program = progNum;
	exec_run();
	return 0;
}

int32_t exec_list()
{
	int i;
	for (i=0; g_progTable[i]; i++)
		cprintf("%d = %s: %s\n", i, g_progTable[i]->progName, g_progTable[i]->desc);

 	return 0;
}

int videoSetup();
int videoLoop();

Program g_progVideo =
{
	"video",
	"continuous stream of raw camera frames",
	videoSetup, 
	videoLoop
};

int videoSetup()
{
	return 0;
}

int videoLoop()
{
	static int i = 0;
	cprintf("hello %d\n", i++);
	cam_getFrameChirp(0x21, 0, 0, 320, 200, g_chirpUsb);

	return 0;
}

int blobsSetup();
int blobsLoop();

Program g_progBlobs =
{
	"blobs",
	"detect of colored object and return them as blobs",
	blobsSetup, 
	blobsLoop
};

extern Qqueue *g_qqueue;

int blobsSetup()
{
	int responseInt;

	cam_setMode(CAM_MODE1);

	g_chirpM0->callSync(g_runM0, UINT8(0), END_OUT_ARGS,
		&responseInt, END_IN_ARGS);
}

int blobsLoop()
{
	static int i=0;
	Qval qval;

	while(g_qqueue->dequeue(&qval))
	{
		if (qval==0xffffffff)
			cprintf("%d\n", i++);	
	}
}

void exec_loop()
{
	int responseInt;

	while(1)
	{
		// wait for program to start
		while(!g_run)
		{
			g_chirpUsb->service();
			if (!g_chirpUsb->connected() || !USB_Configuration)
				exec_run();
		}

		// setup
		if ((*g_progTable[g_program]->setup)()<0)
			g_run = false; // setup failed!

		// loop
		while(g_run)
		{
			if ((*g_progTable[g_program]->loop)()<0)
				break; // loop failed!	
			while(g_chirpUsb->service());
		}

		// set variable to indicate we've stopped
		g_run = false;
		g_running = false;
		// stop M0
		g_chirpM0->callSync(g_stopM0, END_OUT_ARGS,
			&responseInt, END_IN_ARGS);
	}
}
