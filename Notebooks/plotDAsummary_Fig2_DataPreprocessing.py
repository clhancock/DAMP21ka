
import os
wd = '/Users/chrishancock/Library/CloudStorage/OneDrive-NorthernArizonaUniversity/Research/Manuscript/DAMP21k/' #changed
os.chdir(wd+'Holocene-code') #changed
import csv
import sys
import numpy as np
import yaml
import time
import datetime
import netCDF4
import da_utils
import da_utils_lmr
import da_load_models
import da_load_proxies
import da_psms



#%% SETTINGS

starttime_total = time.time() # Start timer

# Use a given config file.  If not given, use config_default.yml.
if len(sys.argv) > 1: config_file = sys.argv[1]
#else:                 config_file = 'config.yml'
else:                 config_file = 'config_default.yml'

# Load the configuration options and print them to the screen.
print('Using configuration file: '+config_file)
with open(config_file,'r') as file: options = yaml.load(file,Loader=yaml.FullLoader)

print('=== SETTINGS ===')
for key in options.keys():
    print('%30s: %-15s' % (key,str(options[key])))
print('=== END SETTINGS ===')


options['exp_name_long'] = options['exp_name']+'.'+str(options['localization_radius'])+'loc.'+str(options['prior_window'])+'window.'+str(options['time_resolution'])+'.'
#%% LOAD AND PROCESS DATA

# Load the chosen proxy data
proxy_ts,collection_all = da_load_proxies.load_proxies(options)
proxy_data = da_load_proxies.process_proxies(proxy_ts,collection_all,options)

# Load the chosen model data
model_data = da_load_models.load_model_data(options)

# Detrend the model data if selected
model_data = da_load_models.detrend_model_data(model_data,options)

# Get some dimensions
n_models_in_prior = len(options['models_for_prior'])
n_proxies         = proxy_data['values_binned'].shape[0]

# If the prior is allowed to change through time, remove the mean of the reference period from each model.
if options['reconstruction_type'] == 'relative':
    for i in range(n_models_in_prior):
        ind_for_model = (model_data['number'] == (i+1))
        ind_ref = (model_data['age'] >= options['reference_period'][0]) & (model_data['age'] < options['reference_period'][1]) & ind_for_model
        for var in options['vars_to_reconstruct']:
            model_data[var][ind_for_model,:,:,:]         = model_data[var][ind_for_model,:,:,:]         - np.mean(model_data[var][ind_ref,:,:,:],axis=0)
            model_data[var+'_annual'][ind_for_model,:,:] = model_data[var+'_annual'][ind_for_model,:,:] - np.mean(model_data[var+'_annual'][ind_ref,:,:],axis=0)
            model_data[var+'_jja'][ind_for_model,:,:]    = model_data[var+'_jja'][ind_for_model,:,:]    - np.mean(model_data[var+'_jja'][ind_ref,:,:],axis=0)
            model_data[var+'_djf'][ind_for_model,:,:]    = model_data[var+'_djf'][ind_for_model,:,:]    - np.mean(model_data[var+'_djf'][ind_ref,:,:],axis=0)

# If requested, alter the proxy uncertainty values.
if options['change_uncertainty']:
    if options['change_uncertainty'][0:5] == 'mult_':
        uncertainty_multiplier = float(options['change_uncertainty'][5:])
        proxy_data['uncertainty'] = proxy_data['uncertainty']*uncertainty_multiplier
        print(' --- Processing: All uncertainty values multiplied by '+str(uncertainty_multiplier)+' ---')
    elif options['change_uncertainty'][0:4] == 'all_':
        prescribed_uncertainty = float(options['change_uncertainty'][4:])
        proxy_data['uncertainty'][:] = prescribed_uncertainty
        print(' --- Processing: All uncertainty values set to '+str(prescribed_uncertainty)+' ---')
    else:
        # If using this option, the text file below should contain TSids and MSE for every proxy record
        print(' --- Processing: All uncertainty values set to values from the following file ---')
        print(options['change_uncertainty'])
        proxy_uncertainties_from_file = np.genfromtxt(options['change_uncertainty'],delimiter=',',dtype='str')
        #
        for i in range(n_proxies):
            index_uncertainty = np.where(proxy_data['metadata'][i,1] == proxy_uncertainties_from_file[:,0])[0]
            if len(index_uncertainty) == 0:
                print('No prescribed error value in file for proxy '+str(i)+', TSid: '+str(proxy_data['metadata'][i,1])+'.  Setting to NaN.')
                proxy_data['uncertainty'][i] = np.nan
            else:
                proxy_data['uncertainty'][i] = proxy_uncertainties_from_file[index_uncertainty,1].astype(float)

# Use PSMs to get model-based proxy estimates
proxy_estimates_all,_ = da_psms.psm_main(model_data,proxy_data,options)

#%%
import matplotlib.pyplot   as plt         # Packages for making figures
import matplotlib.gridspec as gridspec
SMALL_SIZE = 8
MEDIUM_SIZE = 10
BIGGER_SIZE = 12


plt.rc('font', size=SMALL_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=SMALL_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=SMALL_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=SMALL_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize
plt.rc('figure', titlesize=MEDIUM_SIZE)  # fontsize of the figure title
plt.rcParams["font.family"] = 'sans-serif'
plt.rcParams['font.sans-serif'] = 'Lucida Grande'

#tsid1 = 'OLS_Rtetfy6NJzz'; tsid2 = 'pdRNyQeqaj6PkSQYY4b'
tsid1 = 'OLS_RMkgfrBZ0gE'; tsid2 = 'pdRAD0OoGMqGUoHfFHZ'

c1 = '#1D1160'; c2 = '#00788C'; s=12
m1 = 's'; m2 = '^'

fig = plt.figure(figsize=(6, 2.25),dpi=400,tight_layout=True)
gs = gridspec.GridSpec(1, 2)
#plt.suptitle('Example data processing for two data types [Pyramid Lake (Nevada, USA)]')
#Original Data
ts1 = ''; ts2 = ''
for i in range(len(proxy_data['metadata'])):
    if   proxy_ts[i]['paleoData_TSid'] == tsid1: ts1 = i
    elif proxy_ts[i]['paleoData_TSid'] == tsid2: ts2 = i

ax1 = plt.subplot(gs[0])
#X axis
ax1.set_xlim(21000,-100); 
ax1.set_xticks(range(-50,22000,3000)); 
ax1.set_xticklabels(['0','','6','','12','','18',''])
ax1.set_xlabel('Age (ka BP)')
ax2 = ax1.twinx()
ax1.spines[['top']].set_visible(False)
ax2.spines[['top']].set_visible(False)
#Y axis
ax1.set_yticks([-3,-2,-1])
ax1.set_yticklabels(['low','mid','high'],color=c1)
ax2.set_yticks(range(1150,1351,75)); 
ax1.tick_params(axis='y', colors=c1)
ax2.tick_params(axis='y', colors=c2)
ax1.yaxis.label.set_color(c1)
#ax2.yaxis.label.set_color(c2)
ax2.spines['left'].set_color(c1)
ax2.spines['right'].set_color(c2)
ax1.set_ylabel('lake level ('+proxy_ts[ts1]['paleoData_units']+')')
ax2.set_ylabel('lake level ('+proxy_ts[ts2]['paleoData_units']+')')
#Plot
ax1.scatter(proxy_ts[ts1]['age'], proxy_ts[ts1]['paleoData_values'],c=c2,ec='k',lw=0.2,marker=m2,label='L&S (2020)',s=s,zorder=2)
#
#ax2.step(proxy_ts[ts2]['age'], proxy_ts[ts2]['paleoData_values'],c=c2,lw=1,zorder=1,where='mid')
ax1.step(proxy_ts[ts1]['age'], proxy_ts[ts1]['paleoData_values'],c=c1,lw=1,zorder=1,where='mid')


ax2.scatter(proxy_ts[ts2]['age'], proxy_ts[ts2]['paleoData_values'],c=c2,ec='k',lw=0,marker=m2,label='L&S (2020)',s=0,zorder=2)
ax1.scatter(proxy_ts[ts1]['age'], proxy_ts[ts1]['paleoData_values'],c=c1,ec='k',lw=0.4,marker=m1,label='OLS (1989)',s=s,zorder=2)
#Final ax adjustments
ax1.legend(markerscale=1.4,borderpad=0.3,handletextpad=0.1)
ax1.set_title('(a) original published data')

#Processed Data
ts1 = ''; ts2 = ''
for i in range(len(proxy_data['metadata'])):
    if   proxy_data['metadata'][i][1] == tsid1: ts1 = i
    elif proxy_data['metadata'][i][1] == tsid2: ts2 = i
    
ax3 = plt.subplot(gs[1])
#X axis
ax3.invert_xaxis()
ax3.set_xlim(21000,-100); 
ax3.set_xticks(range(-50,22000,3000)); 
ax3.set_xticklabels(['0','','6','','12','','18',''])
ax3.set_xlabel('age (ka BP)')
ax3.spines[['left','top']].set_visible(False)
#Y axis
ax3.set_ylabel('lake status (percentile)')
ax3.yaxis.set_label_position("right")
ax3.yaxis.tick_right()
# #Plot
ax3.step(proxy_data['age_centers'], proxy_data['values_binned'][ts1],c=c1,lw=1, zorder=1)
ax3.step(proxy_data['age_centers'], proxy_data['values_binned'][ts2],c=c2,lw=1, zorder=1)
ax3.scatter(proxy_data['age_centers'], proxy_data['values_binned'][ts2],c=c2,ec='k',lw=0.4,marker=m2,label='L&S (2020)',s=s,zorder=2)
ax3.scatter(proxy_data['age_centers'], proxy_data['values_binned'][ts1],c=c1,ec='k',lw=0.4,marker=m1,label='OLS (1989)',s=s,zorder=2)

#Final ax adjustments
ax3.legend(markerscale=1.4,borderpad=0.3,handletextpad=0.1)
ax3.set_title('(b) binned & converted to percentile units')
#Final Plot adjustments
#plt.savefig(wd+'Figures/Fig2. ExampleLakeProcessing.png',dpi=600)
#%%
url = 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2086_2099_CONUS_daily.nc'
from netCDF4 import Dataset
link = Dataset(url)
link
#%%
link










#%%