dividendAddr = 0x10
divisorAddr = 0x11
quotientAddr = 0x12

squaredError = 0
passes = 0
fails = 0

function testMath()
	dividend = emu.read(dividendAddr, emu.memType.nesDebug)
	divisor = emu.read(divisorAddr, emu.memType.nesDebug)
	quotient = emu.read(quotientAddr, emu.memType.nesDebug)
	quotientReal = math.floor(dividend * 256 / divisor)
	
	squaredError = squaredError + (quotient - quotientReal) * (quotient - quotientReal)
	if (quotient ~= quotientReal) then
		emu.log(dividend .. " / " .. divisor .. " = " .. quotient .. " (" .. quotientReal .. ")")
		fails = fails + 1
	else
		passes = passes + 1
	end
	
	emu.drawString(0, 0, "Dividend: " .. dividend .. "    \nDivisor: " .. divisor .. "    \nPasses: " .. passes .. "    \nFails: " .. fails .. "    \nMean Squared Error: " .. squaredError / (passes + fails) .. "    ", 0xFFFFFF, 0x000000, 0, 0)
end

emu.addMemoryCallback(testMath, emu.callbackType.write, 0x4444)