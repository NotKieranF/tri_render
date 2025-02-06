-- Get pattern buffer labels
patternBufferLabels = {}
for i = 0, 7 do
	patternBufferLabels[i] = emu.getLabelAddress(string.format("pattern_buffer_%d", i))
end

--
nametableBufferLabel = emu.getLabelAddress("nametable_buffer")
NAMETABLE_WIDTH = 32
NAMETABLE_HEIGHT = 30
nextPartialPatternIndexLabel = emu.getLabelAddress("next_partial_pattern_index")

function transferBuffers()
	-- Write blank tile
	for i = 0, 15 do
		emu.write(0x0000 + i, 0x00, emu.memType.nesPpuMemory)
	end

	-- Copy over partial patterns
	local allocatedPartialPatterns = emu.read(nextPartialPatternIndexLabel.address, nextPartialPatternIndexLabel.memType)
	for i = 1, allocatedPartialPatterns do
		for j = 0, 7 do
			local cpuAddress = patternBufferLabels[j].address + i
			local ppuAddress = 0x0000 + i * 16 + j
			emu.write(ppuAddress + 0, emu.read(cpuAddress, patternBufferLabels[j].memType), emu.memType.nesPpuMemory)
			emu.write(ppuAddress + 8, 0x00, emu.memType.nesPpuMemory)
		end
	end
	
	-- Copy over nametable
	for i = 0, NAMETABLE_WIDTH * NAMETABLE_HEIGHT do
		local cpuAddress = nametableBufferLabel.address + i
		local ppuAddress = 0x2000 + i
		emu.write(ppuAddress, emu.read(cpuAddress, nametableBufferLabel.memType), emu.memType.nesPpuMemory)
	end
end

emu.addMemoryCallback(transferBuffers, emu.callbackType.write, 0x5555)