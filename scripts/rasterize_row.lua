rasterize_row_callback = 0x4448
left_edges = emu.getLabelAddress("left_edges")["address"]
right_edges = emu.getLabelAddress("right_edges")["address"]
tile_row = 0x08

function rasterize_row()
	poly_ptr = emu.read16(0x00, emu.memType.nesMemory)
	poly_len = emu.read(poly_ptr + 0, emu.memType.nesMemory)
	for i=2,poly_len,2 do
		x = emu.read(poly_ptr + i + 0, emu.memType.nesMemory)
		y = emu.read(poly_ptr + i + 1, emu.memType.nesMemory)
		emu.log(i .. ", " .. x .. ", " .. y)
		emu.drawPixel(x, y, 0xFF0000)
	end

	for i=0,7 do
		left = emu.read(left_edges + i, emu.memType.nesMemory)
		right = emu.read(right_edges + i, emu.memType.nesMemory)
		row = emu.read(tile_row, emu.memType.nesMemory) * 8
		if (left > right) then
--			emu.log(i)
			emu.drawLine(left, row + i, right, row + i)
		end
	end
end

emu.addMemoryCallback(rasterize_row, emu.callbackType.write, rasterize_row_callback)