import math

def toWord(value):
	if value < 0:
		value = (abs(value) ^ 0xFFFF) + 1
	return "$" + hex(value).lstrip("0x").upper().rjust(4, "0")

def toByte(value):
	if value < 0:
		value = (abs(value) ^ 0xFF) + 1
	return "$" + hex(value).lstrip("0x").upper().rjust(2, "0")

def writeRow(file, values):
	file.write(".BYTE	")
	
	for i in values:
		file.write(f"{toByte(i)}, ")
	
	file.write("\n")
	return

def writeLabel(file, label):
	file.write(f"{label}:")

def writeTable(file, label, values):
	writeLabel(file, label)
	for i in range(len(values)):
		if (i % 16 == 0):
			file.write(f"\n.BYTE	{toByte(values[i])}")
		else:
			file.write(f", {toByte(values[i])}")
	return

def generateSin():
	return

with open("src/includes/trig_tables.inc", "w") as file:
	sin = []
	hsin = []
	qsin = []
	esin = []
	cos = []
	hcos = []
	qcos = []
	ecos = []
	for i in range(0, 256):
		sinVal = round(math.sin(i * math.pi / 128) * 127)
		sin.append(sinVal)
		hsin.append(round(sinVal / 2))
		qsin.append(round(sinVal / 4))
		esin.append(round(sinVal / 8))

		cosVal = round(math.cos(i * math.pi / 128) * 127)
		cos.append(cosVal)
		hcos.append(round(cosVal / 2))
		qcos.append(round(cosVal / 4))
		ecos.append(round(cosVal / 8))


	writeTable(file, "sin", sin)
	file.write("\n\n")
	writeTable(file, "hsin", hsin)
	file.write("\n\n")
	writeTable(file, "qsin", qsin)
	file.write("\n\n")
	writeTable(file, "esin", esin)
	file.write("\n\n")

	writeTable(file, "cos", cos)
	file.write("\n\n")
	writeTable(file, "hcos", hcos)
	file.write("\n\n")
	writeTable(file, "qcos", qcos)
	file.write("\n\n")
	writeTable(file, "ecos", ecos)
