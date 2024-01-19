import xml.etree.ElementTree as ET

tree = ET.parse("assets/maps/concrete/0.tmx")
root = tree.getroot()

for child in root:
	print(child.tag, child.attrib)

#