test_complete_callback = 0x4444

function test_udiv_8x8bit_frac()
	dividend = emu.read(0xFE, emu.memType.nesMemory)
	divisor = emu.read(0xFF, emu.memType.nesMemory)
	remainder = emu.read(0xEE, emu.memType.nesMemory)
	quotient = emu.read(0xEF, emu.memType.nesMemory)
	
	real_remainder = dividend % divisor
	real_quotient = math.floor((dividend / divisor) * 256)
	if ((real_remainder ~= remainder) or (real_quotient ~= quotient)) then
		emu.log(dividend .. "/" .. divisor .. " = " .. real_quotient .. " r " .. real_remainder .. ", not " .. quotient .. " r " .. remainder)
	end
end

emu.addMemoryCallback(test_udiv_8x8bit_frac, emu.callbackType.write, test_complete_callback)