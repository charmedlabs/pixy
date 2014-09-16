################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include src/subdir.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
endif

-include ../makefile.defs

# Add inputs and outputs from these tool invocations to the build variables 

# All Target
all: m0.axf

# Tool invocations
m0.axf: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: MCU Linker'
	arm-none-eabi-gcc -nostdlib -L"C:\CharmedLabs\cmucam\git\device\gcc\pixy_m0\Release" -Xlinker -Map="m0.map" -Xlinker --gc-sections -mcpu=cortex-m0 -mthumb -T "m0_Release.ld" -o "m0.axf" $(OBJS) $(USER_OBJS) $(LIBS) && arm-none-eabi-objcopy --target elf32-littlearm --verbose --strip-all --redefine-sym __vectors_start__=__vectors_start___core_m0app --keep-symbol __vectors_start___core_m0app --rename-section .text=".core_m0app" --rename-section .data=".core_m0app.data" --rename-section .data_RAM2=".core_m0app.data_RAM2" --rename-section .data_RAM3=".core_m0app.data_RAM3" --rename-section .data_RAM4=".core_m0app.data_RAM4" --rename-section .data_RAM5=".core_m0app.data_RAM5" "m0.axf" "m0.axf.o"
	@echo 'Finished building target: $@'
	@echo ' '
	$(MAKE) --no-print-directory post-build

# Other Targets
clean:
	-$(RM) $(OBJS)$(C_DEPS)$(EXECUTABLES) m0.axf
	-@echo ' '

post-build:
	-@echo 'Performing post-build steps'
	-arm-none-eabi-size "m0.axf"; # arm-none-eabi-objcopy -v -O binary "m0.axf" "m0.bin" ; # checksum -p LPC4330_M0 -d "m0.bin";
	-@echo ' '

.PHONY: all clean dependents
.SECONDARY: post-build

-include ../makefile.targets
