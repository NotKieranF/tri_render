multiplierAddr = 0x10
multiplicandAddr = 0x11
productAddr = 0x13

squaredError = 0
passes = 0
fails = 0

function checkMath()
	multiplier = emu.read(multiplierAddr, emu.memType.nesDebug)
	multiplicand = emu.readWord(multiplicandAddr, emu.memType.nesDebug, true)
	product = emu.readWord(productAddr, emu.memType.nesDebug, true)
	productReal = math.floor(multiplier * multiplicand / 256)
	
	squaredError = squaredError + (product - productReal) * (product - productReal)
	if (product ~= productReal) then
		emu.log(multiplicand .. " x " .. multiplier .. " = " .. product .. " (" .. productReal .. ")")
		fails = fails + 1
	else
		passes = passes + 1
	end
	
	emu.drawString(0, 0, "Multiplicand: " .. multiplicand .. "    \nMultiplier: " .. multiplier .. "    \nPasses: " .. passes .. "    \nFails: " .. fails .. "    \nMean Squared Error: " .. squaredError / (passes + fails) .. "    ")
end

emu.addMemoryCallback(checkMath, emu.callbackType.write, 0x4444)