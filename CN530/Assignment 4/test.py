from numpy import sin,cos
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

class DoubleSprings:
	"""double pendulum made of two springs"""
	def __init__(self, init_state = [1.0,0,1.0,0,120,0,-20,0],#L1, vL1, L2, vL2, Theta1, vTheta1, Theat2, vTheta2
			L1o=1.0,#unstretched length of spring 1
			L2o=1.0,#unstreatched length of spring 2
			M1=1.0,#mass of pend 1
			M2=1.0,#mass of pend2
			K1=50.,#spring constant pend1
			K2=50.,#spring constant pend2
			G=9.8,#accel due to grav
			origin=(0,0)):
		self.init_state = np.asarray(init_state,dtype='float')
		self.params = (L1o,L2o,M1,M2,K1,K2,G)
		self.origin = origin
		self.time_elapsed = 0
		self.state = self.init_state*np.pi/180. #convert to radians
		self.state[0]=self.init_state[0]
		self.state[1]=self.init_state[1]
		self.state[3]=self.init_state[3]
		self.state[2]=self.init_state[2]
	def setPosition(self):
		(L1o,L2o,M1,M2,K1,K2,G)=self.params
		"""gets current xy pos from the state"""
		x=np.cumsum([self.origin[0],self.state[0]*sin(self.state[4]),self.state[2]*sin(self.state[6])])
		y=np.cumsum([self.origin[1],-self.state[0]*cos(self.state[4]),-self.state[2]*cos(self.state[6])])
		return x,y
	def eulerAdvance(self,dt):
		(L1o,L2o,M1,M2,K1,K2,G)=self.params
		if(self.state[0]<.002):
			self.state[0]=.002
			self.state[1]=0.
			self.state[5]=0.
		if(self.state[0]>8.):
			self.state[0]=8.
			self.state[1]=0.
		if(self.state[2]>8.):
			self.state[2]=8.
			self.state[3]=0.
		if(self.state[2]<.001):
			self.state[2]=.001
			self.state[3]=0.
			self.state[7]=0.
		vels = self.getVelocities(self.state,self.time_elapsed)
		self.state= self.state + dt*vels
		self.time_elapsed = self.time_elapsed + dt
		if(self.state[0]<.002):
			self.state[0]=.002
			self.state[1]=0.
		if(self.state[2]<0.001):
			self.state[2]=.001
			self.state[3]=0.
		if(self.state[0]>8.):
			self.state[0]=8.
			self.state[1]=0.
		if(self.state[2]>8.):
			self.state[2]=9.
			self.state[3]=0.
		self.setPosition()
	def rungeKutta4Advance(self,dt):
		(L1o,L2o,M1,M2,K1,K2,G)= self.params
		state1 = self.state
		vels1 = self.getVelocities(state1,self.time_elapsed)
		
		state2 = state1 + (dt/2.0)*vels1
		vels2 = self.getVelocities(state2,self.time_elapsed + dt/2.0)

		state3 = state2 + (dt/2.0)*vels2
		vels3 = self.getVelocities(state3,self.time_elapsed + dt)

		state4 = state3 + dt*vels3
		vels4 = self.getVelocities(state4,self.time_elapsed + 2*dt)

		velsfinal = (1/6.0)*(vels1 + 2.*vels2 + 2.*vels3 + vels4)
		self.state = self.state + dt*velsfinal
		self.time_elapsed = self.time_elapsed + dt
		self.setPosition()	
	def getVelocities(self,state,t):
		(L1o,L2o,M1,M2,K1,K2,G)=self.params
		vels = np.zeros_like(state)
		vels[0]=state[1]
		vels[2]=state[3]
		vels[4]=state[5]
		vels[6]=state[7]
		#L1=0 L2 = 2 Th1 =4 Th2 =6
		"""now compute velocities from eqns from lagrangian"""
		cos_delta = cos(state[4]-state[6])
		sin_delta = sin(state[4]-state[6])
		#L1''
		vels[1]=(1.0/M1)*(K1*L1o+G*M1*cos(state[4])-K2*L2o*cos_delta+K2*cos_delta*state[2]+state[0]*(-K1+M1*(state[5]**2)))
		#L2''
		vels[3]=(1.0/(M1*M2))*(K2*L2o*M1+K2*L2o*M2-K1*L1o*M2*cos_delta+K1*M2*cos_delta*state[0]-state[2]*(K1*(M1+M2)-M1*M2*(state[7]**2)))
		#th1''
		vels[5]=(-1.0/(state[0]))*(G*M1*sin(state[4])-K2*L2o*sin_delta+K2*state[2]*sin_delta+2*M1*state[1]*state[5])
		#th2''
		vels[7]=(-K1*L1o*sin_delta+K2*state[0]*sin_delta-2.0*M1*state[3]*state[7])/(M1*state[2])
		return vels

springs = DoubleSprings([.5,0.,.5,0.,180.,0.,180.,0.])
dt = 1./100#30 frames per sec
springs2 = DoubleSprings([1.0,0.,1.0,0.,120,100,70,100])
fig = plt.figure()
ax = fig.add_subplot(111,aspect = 'equal', autoscale_on=False,xlim=(-5,5),ylim=(-5,5))
ax.grid()

line, = ax.plot([],[],'o-',lw=2)
line2, = ax.plot([],[],'o-',lw=2)
line2.set_color('black')
time_text = ax.text(.02,0.95,'',transform=ax.transAxes)

def init():
	"""start animation"""
	line.set_data([],[])
	line2.set_data([],[])
	time_text.set_text('')
	return line,line2, time_text

def animate(i):
	global springs,springs2,dt
	springs2.eulerAdvance(dt)
	springs.rungeKutta4Advance(dt)
	line2.set_data(springs2.setPosition())
	line.set_data(springs.setPosition())
	time_text.set_text('time = %.1f'% springs.time_elapsed)
	return line,line2, time_text

from time import time
t0 = time()
animate(0)
t1 = time()
interval = 1000* dt - (t1-t0)
ani = animation.FuncAnimation(fig,animate, frames= 300, interval = interval, blit = True, init_func = init)

plt.show()
