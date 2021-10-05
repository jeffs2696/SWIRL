#!/usr/bin/env python
# coding: utf-8

# In[20]:


import tikzplotlib
import glob
import os
import pandas as pd
import matplotlib as mpl
from matplotlib import pyplot as plt


# In[21]:


mean_flow_file_path= '01-mean-flow/'
MMS_path = '03-method-of-manufactured-solutions/'
data_paths = [mean_flow_file_path,MMS_path]

for i in range(len(data_paths)):
    read_files =     glob.glob( os.path.join(data_paths[i],'*.dat' ))
    


# In[22]:


flow_data = pd.read_csv(                   '01-mean-flow/mean-flow513.dat',                    delim_whitespace= True)


# In[23]:


LEE_L2_data = pd.read_csv(                      '03-method-of-manufactured-solutions/L2-LEE.dat',                       delim_whitespace=True)
LEE_ROC_data = pd.read_csv(                      '03-method-of-manufactured-solutions/ROC-LEE.dat',                       delim_whitespace=True)
SND_L2_data = pd.read_csv(                      '03-method-of-manufactured-solutions/L2-sound_speed-.dat',                       delim_whitespace=True)
SND_ROC_data = pd.read_csv(                      '03-method-of-manufactured-solutions/ROC-sound_speed.dat',                       delim_whitespace=True)


# In[24]:


Delta_r = LEE_ROC_data.Delta_r
LEE_ROC = LEE_ROC_data.ROC
SND_ROC = SND_ROC_data.ROC


# In[30]:


plt.style.use('seaborn-ticks')
# plot data
plt.rcParams
plt.plot(          flow_data['radius'],flow_data['M_x'],          label = '$M_{x}$',         )
plt.plot(         flow_data['radius'],flow_data['M_theta'],          label = '$M_{\\theta}$' ,         )

# add details
plt.legend()
plt.title('Mean Flow ')
plt.ylabel('Mach Number')
plt.xlabel('Radius')
plt.tight_layout()
tikzplotlib.save("plotReport/MachDistribution.tex")


# In[31]:
fig, ax = plt.subplots(1,1)



ax.plot(         flow_data['radius'],flow_data['A_expected'],          label ='Expected' ,         marker='o',markevery=5)
ax.plot(         flow_data['radius'],flow_data['A_actual'],          label ='Actual' ,         marker='o',markevery=10)
ax.set_ylabel('Speed of Sound')
ax.set_xlabel('Radius')
ax.legend()
tikzplotlib.save("plotReport/SoundSpeedFromIntegration.tex")


# In[27]:


fig, (ax1,ax2,ax3,ax4) = plt.subplots(4,1,sharex=True)

ax1.set_title('Perturbation Variables')
ax1.plot(         flow_data['radius'],flow_data['vR']          )
ax1.set_ylabel('Radial')
ax2.plot(         flow_data['radius'],flow_data['vTh']          )
ax2.set_ylabel('Tangential')
ax3.plot(         flow_data['radius'],flow_data['vX']        )
ax3.set_ylabel('Axial')
ax4.plot(         flow_data['radius'],flow_data['Pr']        )
ax4.set_ylabel('Pressure')

tikzplotlib.save("plotReport/PerturbationVariables.tex")


# In[28]:


fig, (ax1,ax2) = plt.subplots(                       nrows=2,                       ncols=1,                              sharex=True)
ax1.semilogy(Delta_r,SND_ROC,label='Speed Of Sound')
ax1.legend()
ax1.set_title('Rate Of Convergence')
ax2.semilogy(Delta_r,LEE_ROC,label='LEE')
ax2.legend()
ax2.set_xlabel('Del r')

tikzplotlib.save("plotReport/ROC.tex")


# In[29]:


#plt.semilogy(Delta_r,LEE_ROC)
fig, ax = plt.subplots(nrows =4, ncols=1,sharex=True,figsize=(10,4))

# can i loop though axes?
ax[0].set_ylabel('Radial')
ax[0].plot(flow_data['radius'],flow_data['S_1_e'],label='expected')
ax[0].plot(flow_data['radius'],flow_data['S_1_a'],label='actual')
ax[0].legend(loc="upper right")
ax[1].set_ylabel('Tangential')
ax[1].plot(flow_data['radius'],flow_data['S_2_e'])
ax[1].plot(flow_data['radius'],flow_data['S_2_a'])
ax[2].set_ylabel('Axial')
ax[2].plot(flow_data['radius'],flow_data['S_3_e'])
ax[2].plot(flow_data['radius'],flow_data['S_3_a'])
ax[3].set_ylabel('Energy')
ax[3].plot(flow_data['radius'],flow_data['S_4_e'])
ax[3].plot(flow_data['radius'],flow_data['S_4_a'])
tikzplotlib.save("plotReport/SourceTermData.tex")


# In[ ]:





# In[ ]:




