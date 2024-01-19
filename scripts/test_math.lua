multiplicandAddr = 0x10
multiplierAddr = 0x11
productAddr = 0x13

fails = 0
passes = 0

function checkMath()
	multiplicand = emu.read(multiplicandAddr, emu.memType.nesDebug, true)
	multiplier = emu.read(multiplierAddr, emu.memType.nesDebug, true)
	product = emu.readWord(productAddr, emu.memType.nesDebug)

	if ((multiplier * multiplicand) % 65536 ~= product) then
		emu.log(multiplicand .. " x " .. multiplier .. " = " .. product .. " (" .. (multiplicand * multiplier) % 65535 .. ")")
		fails = fails + 1
	else
		passes = passes + 1
	end
	emu.drawString(0, 0, "Multiplicand: " .. multiplicand .. "    \nMultiplier: " .. multiplier .. "    \nFails: " .. fails .. "    \nPasses: " .. passes .. "    ")
end



emu.addMemoryCallback(checkMath, emu.callbackType.write, 0x4444)