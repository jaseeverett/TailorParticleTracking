#!/srv/scratch/z9902002/anaconda3/envs/py3_parcels/bin/python

from parcels import FieldSet, Field, AdvectionRK4, ParticleSet, JITParticle, plotTrajectoriesFile, Variable, BrownianMotion2D, random
from parcels import ErrorCode
import numpy as np
from glob import glob
import time as timelib
from datetime import timedelta as delta
from datetime import datetime as datetime
import cartopy
import os
from operator import attrgetter

array_ref = int(os.environ['PBS_ARRAYID'])
#array_ref = 0

data_dir = ' /srv/scratch/z3097808/20year_run/20year_freerun_output_NEWnci/'
out_dir = (os.environ['TMPDIR'])  # number of particles to be released
print(out_dir)

repeatdt = delta(days=1)  # release from the same set of locations every X day
npart = 1000  # number of particles to be released

# Forward: 9
temp_lon_array = np.array([153.8072, 153.5873, 153.5460, 153.6929, 153.7817, 153.7955, 153.7790, 153.7062, 153.5131])
temp_lat_array = np.array([-26.0, -26.5, -27.0, -27.5, -28.0, -28.5, -29.0, -29.50, -30.00])
temp_year_array = np.arange(1994, 2016, 1)

lon_array = np.repeat(temp_lon_array, temp_year_array.size)
lat_array = np.repeat(temp_lat_array, temp_year_array.size)
year_array = np.tile(temp_year_array, temp_lat_array.size)

# Backwards: 13
#lon_array = [150.8550, 151.4167, 152.8444, 150.2451, 153.7313, 153.7861, 148.9148, 150.1600, 150.3833, 153.0958, 153.3182, 153.8036, 153.6422]
#lat_array = [-35.1, -33.8, -32, -36.2, -29.4, -28.1, -38, -37, -35.7, -31.4, -30.4, -28.8, -27.3]

lon = np.repeat(lon_array[array_ref],npart)
lat = np.repeat(lat_array[array_ref],npart)

start_time = datetime(year_array[array_ref],8, 1)
end_time = datetime(year_array[array_ref]+1,5, 15)  #year, month, day,

runtime = end_time-start_time + delta(days=1)

ufiles = sorted(glob('/srv/scratch/z3097808/20year_run/20year_freerun_output_NEWnci/outer_avg_*'))
vfiles = ufiles
tfiles = ufiles
bfiles = 'EACouter_mesh_srho.nc'
mesh_mask = 'EACouter_mesh_srho.nc'

# Set diffusion constants.
Kh_zonal = 100
Kh_meridional = 100

## ######
filenames = {'U': ufiles,
             'V': vfiles,
             'temp': tfiles,
             'bathy': bfiles,
             'mesh_mask': mesh_mask}

variables = {'U': 'u',
             'V': 'v',
             'temp': 'temp',
             'bathy': 'h'}


out_file = str(out_dir)+'/'+str(year_array[array_ref])+'_Lat'+str(lat_array[array_ref])+'_For.nc'

dimensions = {'lon': 'lon_psi', 'lat': 'lat_psi', 'depth': 's_rho', 'time': 'ocean_time'}
dimensions['bathy'] = {'lon': 'lon_rho', 'lat': 'lat_rho'}

indices = {'depth': [29]}

if os.path.exists(out_file):
    os.remove(out_file)

def DeleteParticle(particle, fieldset, time):
    particle.delete()

fieldset = FieldSet.from_nemo(filenames, variables, dimensions, indices, allow_time_extrapolation=True)#, transpose=True)
fieldset.add_constant('maxage', 40.*86400)
fieldset.temp.interp_method = 'nearest'


# Create field of Kh_zonal and Kh_meridional, using same grid as U
#[time, depth, particle.lon, particle.lat] # Think this order is correct for here
size4D = (30,30,fieldset.U.grid.ydim, fieldset.U.grid.xdim)
fieldset.add_field(Field('Kh_zonal', Kh_zonal*np.ones(size4D), grid=fieldset.temp.grid))
fieldset.add_field(Field('Kh_meridional', Kh_meridional*np.ones(size4D), grid=fieldset.temp.grid))

random.seed(123456) # Set random seed


class SampleParticle(JITParticle):         # Define a new particle class
    age = Variable('age', dtype=np.float32, initial=0.) # initialise age
    temp = Variable('temp', dtype=np.float32, initial=fieldset.temp)  # initialise temperature
    bathy = Variable('bathy', dtype=np.float32, initial=fieldset.bathy)  # initialise bathy
    distance = Variable('distance', initial=0., dtype=np.float32)  # the distance travelled
    prev_lon = Variable('prev_lon', dtype=np.float32, to_write=False,
                        initial=attrgetter('lon'))  # the previous longitude
    prev_lat = Variable('prev_lat', dtype=np.float32, to_write=False,
                        initial=attrgetter('lat'))  # the previous latitude.


def SampleDistance(particle, fieldset, time):
    # Calculate the distance in latitudinal direction (using 1.11e2 kilometer per degree latitude)
    lat_dist = (particle.lat - particle.prev_lat) * 1.11e2
    # Calculate the distance in longitudinal direction, using cosine(latitude) - spherical earth
    lon_dist = (particle.lon - particle.prev_lon) * 1.11e2 * math.cos(particle.lat * math.pi / 180)
    # Calculate the total Euclidean distance travelled by the particle
    particle.distance = math.sqrt(math.pow(lon_dist, 2) + math.pow(lat_dist, 2))
    particle.prev_lon = particle.lon  # Set the stored values for next iteration.
    particle.prev_lat = particle.lat
    
def SampleAge(particle, fieldset, time):
    particle.age = particle.age + math.fabs(dt)
    if particle.age > fieldset.maxage:
        particle.delete()

def SampleTemp(particle, fieldset, time):
    particle.temp = fieldset.temp[time, particle.depth, particle.lat, particle.lon]

def SampleBathy(particle, fieldset, time):
    particle.bathy = fieldset.bathy[0, 0, particle.lat, particle.lon]


start_time = np.repeat(start_time,len(lon))

pset = ParticleSet.from_list(fieldset, pclass=SampleParticle, lon=lon, lat=lat, time=start_time, repeatdt=repeatdt)

pfile = pset.ParticleFile(out_file, outputdt=delta(days=1))

kernels = pset.Kernel(AdvectionRK4) + SampleAge + SampleTemp + SampleBathy + BrownianMotion2D + SampleDistance

pset.execute(kernels, 
             dt=delta(minutes=5), 
             output_file=pfile, 
             verbose_progress=False,
             recovery={ErrorCode.ErrorOutOfBounds: DeleteParticle},
			 endtime=end_time)