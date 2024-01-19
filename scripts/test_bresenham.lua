test = emu.getLabelAddress("test_bresenham")
step = emu.getLabelAddress("step_bresenham")
current_x = emu.getLabelAddress("bres_current_x")
current_y = emu.getLabelAddress("bres_current_y")

function plot()
	state = emu.getState()
	side = state["cpu.x"]

	x = emu.read(current_x.address + side, current_x.memType)
	y = emu.read(current_y.address + side, current_y.memType)

	emu.log(side)
	emu.log("(" .. x .. "," .. y .. ")")
	emu.drawPixel(x, y, 0xFFFFFF, 0)
end

emu.addMemoryCallback(plot, emu.callbackType.exec, step.address + 0x8000)