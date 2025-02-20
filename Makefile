# Recursive wildcard
rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

PROJECT_NAME := tri_render
ROM_NAME := $(PROJECT_NAME).nes
DBG_NAME := $(PROJECT_NAME).dbg
MAP_NAME := $(PROJECT_NAME).map
INC_DIR := ./src/includes
BIN_DIR := ./src/bin
CFG_FILE := ./src/nrom.cfg
ASM_ARGS := --cpu 6502X
LINK_ARGS :=
LIBRARIES :=
MESEN_ARGS := --doNotSaveSettings --nes.enableOamDecay=true --nes.enablePpuOamRowCorruption=true --nes.enablePpu2000ScrollGlitch=true --nes.enablePpu2006ScrollGlitch=true --nes.randomizeMapperPowerOnState=true --nes.randomizeCpuPpuAlignment=true --nes.ramPowerOnState=Random


# Search for .asm files throughout the src directory
ASSEMBLY_FILES := $(call rwildcard, src, *.asm)
OBJECT_FILES := $(patsubst %.asm, build/%.o, $(notdir $(ASSEMBLY_FILES)))



.PHONY: clean default dir $(ASSEMBLY_FILES)

default: dir $(ASSEMBLY_FILES)
	@ld65 $(LINK_ARGS) $(OBJECT_FILES) $(LIBRARIES) -C $(CFG_FILE) -m $(MAP_NAME) --dbgfile $(DBG_NAME) -o $(ROM_NAME)

mesen: default
	@Mesen $(ROM_NAME) $(MESEN_ARGS) &

dir:
	@mkdir -p build

$(ASSEMBLY_FILES):
	@ca65 $@ -g -o $(patsubst %.asm, build/%.o, $(notdir $@)) -I $(INC_DIR) --bin-include-dir $(BIN_DIR) $(ASM_ARGS)

clean:
	@rm -rf build
	@rm -f $(ROM_NAME)
	@rm -f $(DBG_NAME)
	@rm -f $(MAP_NAME)