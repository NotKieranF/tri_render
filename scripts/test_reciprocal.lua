reciprocalInputAddr = 0x10
reciprocalOutputAddr = 0x12

fails = 0
passes = 0

function checkReciprocal()
	reciprocalInput = emu.readWord(reciprocalInputAddr, emu.memType.nesDebug)
	reciprocalOutput = emu.readWord(reciprocalOutputAddr, emu.memType.nesDebug)
	reciprocalReal = math.floor(65535 / reciprocalInput)
	
	if (math.abs(reciprocalOutput - reciprocalReal) < 1) then
		passes = passes + 1
	else
		fails = fails + 1
		emu.log("In: " .. reciprocalInput .. " Out: " .. reciprocalOutput .. " Real: " .. reciprocalReal)
	end
	
	--emu.log("Passes: " .. passes .. " Fails: " .. fails)
end

emu.addMemoryCallback(checkReciprocal, emu.callbackType.write, 0x4444)