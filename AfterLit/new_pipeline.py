import sys

import json

import numpy as np

import pickle

from PIL import Image


f = open(f'data.json')

def remove_empty_timesteps(f) :
	raw_data = json.load(f)
	data = {k:v for k,v in raw_data.items() if v}

	json_object = json.dumps(data, indent=4)

	with open("processed.json", "w") as outfile:
		outfile.write(json_object)

	return data

def json_to_list(data) :
	timesteps = []
	for k,v in data.items() :
		timestep = []
		for k_car, car in v.items() :
			timestep.append((car[0], car[1]))
		if len(timestep) > 0 :
			timesteps.append(timestep)
	return timesteps



def trace_routes(data, minc, shape = (200,200)) :
	route_map = np.zeros((int(np.floor(shape[0])), int(np.floor(shape[1]))))
	centered_data = []
	print("Starting route tracing.....")
	for ts in data :
		centered_ts = []
		for car in ts :
			centered_ts.append((car[0], (car[1][0]-minc[0], car[1][1]-minc[1])))
			int_x = int(np.floor(car[1][0]-minc[0]))
			int_y = int(np.floor(car[1][1]-minc[1]))
			route_map[int_x][int_y+1] = 1
			route_map[int_x][int_y] = 1
			route_map[int_x][int_y-1] = 1
			route_map[int_x+1][int_y+1] = 1
			route_map[int_x+1][int_y] = 1
			route_map[int_x+1][int_y-1] = 1
			route_map[int_x-1][int_y+1] = 1
			route_map[int_x-1][int_y] = 1
			route_map[int_x-1][int_y-1] = 1
		centered_data.append(centered_ts)

	return route_map, centered_data

def car_in_sector(coord, x, y, sector_length) :
	if abs(coord[0] - x) <= sector_length and abs(coord[1] - y) <= sector_length :
			return True
	return False

def draw_car(sector, car) :	
	int_x = int(car[1][0])
	int_y = int(car[1][1])  
	if int_x + 1 == sector.shape[0] or int_y + 1 == sector.shape[1] :
		sector[int_x][int_y] = 230
	else :
		sector[int_x][int_y+1] = 230
		sector[int_x][int_y] = 230
		sector[int_x][int_y-1] = 230
		sector[int_x+1][int_y+1] = 230
		sector[int_x+1][int_y] = 230
		sector[int_x+1][int_y-1] = 230
		sector[int_x-1][int_y+1] = 230
		sector[int_x-1][int_y] = 230
		sector[int_x-1][int_y-1] = 230
	return sector

def draw_sectors(sector_map, x, y, sector_length, car=False) :
	for i in range(-sector_length, sector_length):
		for j in range(-sector_length, sector_length):
			if not(i == -sector_length or j == -sector_length or i == sector_length-1 or j == sector_length-1) :
				if car :
					sector_map[x+i,y+j] = 50
				else :
					sector_map[x+i,y+j] = 170
			else : 
				sector_map[x+i,y+j] = 255

	return sector_map


def get_sectors(route_map, sector_length, gap_x, gap_y) :
	horizontal = (route_map.shape[0]-2*sector_length) // (gap_x)
	vertical = (route_map.shape[1]-2*sector_length) // (gap_y)
	sector_map = np.zeros_like(route_map) 
	sectors = []
	s_x = 0; s_y = 0;
	for i in range(horizontal) :
		for j in range(vertical) :
			x = (sector_length+gap_x)*i; y = (sector_length+gap_y)*j
			sector = route_map[-sector_length+x:sector_length+x, -sector_length+y:sector_length+y]

			if np.count_nonzero(sector)> 4*sector_length**2*0.1:
				sectors.append((x,y))
				sector_map = draw_sectors(sector_map, x, y, sector_length)

	route_map = np.multiply(sector_map, route_map)
	# img = Image.fromarray(np.dstack((sector_map,route_map,sector_map)))
	img = Image.fromarray(sector_map)
	img.save('images/sector_map.png')
	# img.show()
	return sectors, sector_map

def get_sector_data(ts, sectors, sector_length):
	average_speed = np.zeros((len(sectors),))
	cars_in_sector = np.ones((len(sectors),))
	detected = 0
	for idx in range(len(sectors)) :
		for car in ts :
			if car_in_sector(car[1], sectors[idx][0], sectors[idx][1], sector_length):
				average_speed[idx] += car[0]
				print(car[0], idx)
				cars_in_sector[idx] += 1
				detected += 1
	
	print(f"Detected {detected} out of {len(ts)} cars !")

	average_speed = np.divide(average_speed, cars_in_sector)
	return average_speed

def color_sectors(ts, sector_map, sectors, sector_length):
	for idx in range(len(sectors)) :	
		for car in ts :
			print(car[1], sectors[idx][0], sectors[idx][1], sector_length)
			if car_in_sector(car[1], sectors[idx][0], sectors[idx][1], sector_length):
				print(f"Drawing car in sector {idx}")
				sector_map = draw_sectors(sector_map, sectors[idx][0], sectors[idx][1], sector_length, car = True)
			else:
			 	sector_map = draw_car(sector_map, car)
	img = Image.fromarray(sector_map)
	img.save('images/car_sector_map.png')
	# img.show()

minc = (-53.11,2)
data = remove_empty_timesteps(f)
list_data = json_to_list(data)
rmap, centered_data = trace_routes(list_data, minc = minc)
rmap= (rmap* 255).astype(np.uint8)
img = Image.fromarray(rmap)
img.save('images/traced_routes.png')
sector_length = 7
sectors, sector_map = get_sectors(rmap, sector_length, 7, 7)

color_sectors(centered_data[30], sector_map, sectors, sector_length)
print(get_sector_data(centered_data[30], sectors, sector_length))

# for idx, car in enumerate(centered_data[30]) :
# 	print(f"Car {idx} with data {car[1]}")
