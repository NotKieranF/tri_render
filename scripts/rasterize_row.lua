rasterize_row_callback = 0x4448
left_edges = emu.getLabelAddress("left_edges")["address"]
right_edges = emu.getLabelAddress("right_edges")["address"]
tile_row_addr = 0x08
target_relative_y_l_addr = 0x11
target_relative_y_r_addr = 0x12
target_x_l_addr = 0xD
target_x_r_addr = 0xE

function rasterize_row()
	row = emu.read(tile_row_addr, emu.memType.nesMemory) * 8
	target_x_l = emu.read(target_x_l_addr, emu.memType.nesMemory)
	target_x_r = emu.read(target_x_r_addr, emu.memType.nesMemory)
	target_relative_y_l = emu.read(target_relative_y_l_addr, emu.memType.nesMemory)
	target_relative_y_r = emu.read(target_relative_y_r_addr, emu.memType.nesMemory)

	for i=0,30 do
		emu.drawLine(0, i * 8, 255, i * 8, 0x0000FF)
	end

	for i=0,7 do
		left = emu.read(left_edges + i, emu.memType.nesMemory)
		right = emu.read(right_edges + i, emu.memType.nesMemory)
		if (left <= right) then
			emu.drawLine(left, row + i, right, row + i)
		end
		if (left > right) then
--			emu.drawLine(left, row + i, right, row + i, 0xFF0000)
		end
	end

--	emu.drawPixel(target_x_l, row + 8 + target_relative_y_l, 0xFF00FF)
--	emu.drawPixel(target_x_r, row + 8 + target_relative_y_r, 0xFF00FF)

	poly_ptr = emu.read16(0x00, emu.memType.nesMemory)
	poly_len = emu.read(poly_ptr + 0, emu.memType.nesMemory)
	for i=2,poly_len,2 do
		x = emu.read(poly_ptr + i + 0, emu.memType.nesMemory)
		y = emu.read(poly_ptr + i + 1, emu.memType.nesMemory)
		emu.log(i .. ", " .. x .. ", " .. y)
		emu.drawPixel(x, y, 0xFF0000)
	end

end

emu.addMemoryCallback(rasterize_row, emu.callbackType.write, rasterize_row_callback)