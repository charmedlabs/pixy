all: spifi

.PHONY: clean
clean:
	$(MAKE) -f makefile_alt clean -C libpixy_m0/Release	
	$(MAKE) -f makefile_alt clean -C main_m0/Release		
	$(MAKE) -f makefile_alt clean -C libpixy_m4/Release	
	$(MAKE) -f makefile_alt clean -C main_m4/SPIFI	
	$(MAKE) -f makefile_alt clean -C main_m4/RAM	

.PHONY: build_this_first
build_this_first:
	$(MAKE) -C build_this_first
	
.PHONY: libpixy_m0
libpixy_m0:
	$(MAKE) -f makefile_alt -C libpixy_m0/Release	

.PHONY:  libpixy_m4
libpixy_m4:
	$(MAKE) -f makefile_alt -C libpixy_m4/Release	

.PHONY: main_m0
main_m0:
	$(MAKE) -f makefile_alt -C main_m0/Release		

.PHONY: ram
ram: build_this_first libpixy_m0 main_m0 libpixy_m4 
	$(MAKE) -f makefile_alt -C main_m4/RAM

.PHONY: spifi
spifi: build_this_first libpixy_m0 main_m0 libpixy_m4 
	$(MAKE) -f makefile_alt -C main_m4/SPIFI
	


	

