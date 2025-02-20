trigger = 0x444F

function plot_point()
	state = emu.getState()
	x = state["cpu.x"]
	y = state["cpu.y"]
	emu.drawPixel(x, y, 0xFFFFFF, 1)
	emu.log("x: " .. x .. " y: " .. y)
end

emu.addMemoryCallback(plot_point, emu.callbackType.write, trigger)