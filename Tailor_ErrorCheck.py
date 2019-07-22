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

# Laptop or Erik/Phil
data_dir = '/Users/jason/UNSW/Projects/TailorParticleTracking/'
array_ref = 0


ufiles = sorted(glob(data_dir+'outer_avg_*'))
vfiles = ufiles
tfiles = ufiles

bfiles = 'EACouter_mesh_srho.nc'
mesh_mask = 'EACouter_mesh_srho.nc'



filenames = {'U': ufiles,
             'V': vfiles,
             'temp': tfiles,
             'bathy': bfiles,
             'mesh_mask': mesh_mask}

variables = {'U': 'u',
             'V': 'v',
             'temp': 'temp',
             'bathy': 'h'}


#array_ref = int(os.environ['PBS_ARRAYID']) # For Katana
array_ref = 0

lon_array = [154] #[153.57]
lat_array = [-32]

    
out_file = 'Tailor_Parcels_TestOutput.nc'   
#out_file = 'Tailor_Parcels_Output_Lat'+str(abs(lat_array[array_ref]))+'_Laptop.nc'

dimensions = {'lon': 'lon_psi', 'lat': 'lat_psi', 'depth': 's_rho', 'time': 'ocean_time'}
dimensions['bathy'] = {'lon': 'lon_rho', 'lat': 'lat_rho'}

indices = {'depth': [29]}

if os.path.exists(out_file):
    os.remove(out_file)

def DeleteParticle(particle, fieldset, time):
    particle.delete()

fieldset = FieldSet.from_nemo(filenames, variables, dimensions, indices, allow_time_extrapolation=True)#, transpose=True)
fieldset.add_constant('maxage', 40.*86400)


npart = 1  # number of particles to be released
repeatdt = delta(days = 1) # release from the same set of locations every delta

# Look at diffusivity in Okubo 1976
# Set diffusion constants.
Kh_zonal = 100
Kh_meridional = 100

# Create field of Kh_zonal and Kh_meridional, using same grid as U

#[time, depth, particle.lon, particle.lat] # Think this order is correct for here
size4D = (30,30,fieldset.U.grid.ydim, fieldset.U.grid.xdim)
fieldset.add_field(Field('Kh_zonal', Kh_zonal*np.ones(size4D), grid=fieldset.temp.grid))
fieldset.add_field(Field('Kh_meridional', Kh_meridional*np.ones(size4D), grid=fieldset.temp.grid))

random.seed(123456) # Set random seed


class SampleParticle(JITParticle):         # Define a new particle class
    age = Variable('age', dtype=np.float32, initial=0.) # initialise age
    temp = Variable('temp', dtype=np.float32, initial=fieldset.temp)  # initialise temperature
    bathy = Variable('bathy', dtype=np.float32, initial=fieldset.bathy)  # initialise temperature    
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
    particle.distance += math.sqrt(math.pow(lon_dist, 2) + math.pow(lat_dist, 2))
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

time0 = fieldset.U.grid.time[0]

lon = lon_array[array_ref] * np.ones(npart)
lat = lat_array[array_ref] * np.ones(npart)

time = [time0] * len(lon)
pset = ParticleSet.from_list(fieldset, pclass=SampleParticle, lon=lon, lat=lat, time=time, repeatdt=repeatdt)
pfile = pset.ParticleFile(out_file, outputdt=delta(days=1))
kernels = pset.Kernel(AdvectionRK4) + SampleAge + SampleTemp + SampleBathy + SampleDistance + BrownianMotion2D

pset.execute(kernels, 
             dt=delta(minutes=5),
             verbose_progress=True,
             runtime=delta(days=100),
             recovery={ErrorCode.ErrorOutOfBounds: DeleteParticle},
             output_file=pfile)
