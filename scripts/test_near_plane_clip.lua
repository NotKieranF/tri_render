prevVertexAddr = 0x14
curVertexAddr = 0x15
meshCameraspaceXAddr = 0x06
meshCameraspaceYAddr = 0x08
meshCameraspaceZAddr = 0x0A
transformedVertexCacheXAddr = emu.getLabelAddress("transformed_vertex_cache_x")["address"]
transformedVertexCacheYAddr = emu.getLabelAddress("transformed_vertex_cache_y")["address"]
transformedVertexCacheZAddr = emu.getLabelAddress("transformed_vertex_cache_z")["address"]
polyBufferXLoAddr = emu.getLabelAddress("poly_buffer_x_lo")["address"]
polyBufferXHiAddr = emu.getLabelAddress("poly_buffer_x_hi")["address"]
polyBufferYLoAddr = emu.getLabelAddress("poly_buffer_y_lo")["address"]
polyBufferYHiAddr = emu.getLabelAddress("poly_buffer_y_hi")["address"]
polyBufferLastAddr = emu.getLabelAddress("poly_buffer_last")["address"]

xDiffMax = 0
xDiffMin = 0
yDiffMax = 0
yDiffMin = 0
zDiffMax = 0
zDiffMin = 0
clipDiffMin = 0
clipDiffMax = 0

function main()
	prevVertex = emu.read(prevVertexAddr, emu.memType.nesDebug)
	curVertex = emu.read(curVertexAddr, emu.memType.nesDebug)
	meshCameraspaceX = emu.readWord(meshCameraspaceXAddr, emu.memType.nesDebug, true)
	meshCameraspaceY = emu.readWord(meshCameraspaceYAddr, emu.memType.nesDebug, true)
	meshCameraspaceZ = emu.readWord(meshCameraspaceZAddr, emu.memType.nesDebug, true)
	polyBufferLast = emu.read(polyBufferLastAddr, emu.memType.nesDebug)

	x1 = emu.read(transformedVertexCacheXAddr + prevVertex, emu.memType.nesDebug, true)
	x2 = emu.read(transformedVertexCacheXAddr + curVertex, emu.memType.nesDebug, true)
	y1 = emu.read(transformedVertexCacheYAddr + prevVertex, emu.memType.nesDebug, true)
	y2 = emu.read(transformedVertexCacheYAddr + curVertex, emu.memType.nesDebug, true)
	z1 = emu.read(transformedVertexCacheZAddr + prevVertex, emu.memType.nesDebug, true)
	z2 = emu.read(transformedVertexCacheZAddr + curVertex, emu.memType.nesDebug, true)
	
	xDiff = math.floor((x1 - x2) / 2)
	if (xDiff < xDiffMin) then xDiffMin = xDiff end
	if (xDiff > xDiffMax) then xDiffMax = xDiff end
	emu.log("xDiffMin: " .. xDiffMin .. " xDiffMax: " .. xDiffMax)

	yDiff = math.floor((y1 - y2) / 2)
	if (yDiff < yDiffMin) then yDiffMin = yDiff end
	if (yDiff > yDiffMax) then yDiffMax = yDiff end
	emu.log("yDiffMin: " .. yDiffMin .. " yDiffMax: " .. yDiffMax)

	zDiff = math.floor((z1 - z2) / 2)
	if (zDiff < zDiffMin) then zDiffMin = zDiff end
	if (zDiff > zDiffMax) then zDiffMax = zDiff end
	emu.log("zDiffMin: " .. zDiffMin .. " zDiffMax: " .. zDiffMax)

	clipDiff = 0x0100 - z1 - meshCameraspaceZ
	if (clipDiff < clipDiffMin) then clipDiffMin = clipDiff end
	if (clipDiff > clipDiffMax) then clipDiffMax = clipDiff end
	emu.log("clipDiffMin: " .. clipDiffMin .. " clipDiffMax: " .. clipDiffMax)

	interp = clipDiff * math.floor(32767 / zDiff) / 32768
	xi = math.floor(meshCameraspaceX + x1 + xDiff * interp + 128)
	yi = math.floor(meshCameraspaceY + y1 + yDiff * interp + 120)
	
	emu.write(polyBufferXLoAddr + polyBufferLast, (xi >> 0) & 0xFF, emu.memType.nesDebug)
	emu.write(polyBufferXHiAddr + polyBufferLast, (xi >> 8) & 0xFF, emu.memType.nesDebug)
	emu.write(polyBufferYLoAddr + polyBufferLast, (yi >> 0) & 0xFF, emu.memType.nesDebug)
	emu.write(polyBufferYHiAddr + polyBufferLast, (yi >> 8) & 0xFF, emu.memType.nesDebug)

	emu.log(xi .. ", " .. yi)
end



emu.addMemoryCallback(main, emu.callbackType.write, 0x4448)