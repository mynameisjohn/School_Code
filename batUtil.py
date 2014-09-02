# 
# 
# File: batUtil.py
#
# Description: A script that generates a graphical user interface designed for
# easy interaction with a scale model of the Tadarida Brasiliensis bat. The GUI allows
# for posing of the model at 18 specific keyframes during its wing cycle, and also allows
# for rendering of both the entire cycle and individual frames. 
# 
# Author: John Joseph
#

import maya.cmds as cmds

def getFormat(intFormat):
	return {
		0: 'gif',
        1: 'pic',
		2: 'rla',
		3: 'tif',
		4: 'tif',
		5: 'sgi',
		6: 'als',
		7: "iff",
		8: "jpg",
		9: "eps",
		10:"iff",
		11:"cin",
		12:"yuv",
		13:"sgi",
		19:"tga",
		20:"bmp",
		22:"mov",
		30:"pntg",
		31:"psd",
		32:"png",
		33:"pict",
		34:"qtif",
		35:"dds",
		36:"psd",
		}.get(intFormat,'png')
		
def safeBatchRender(startFrame,endFrame):
	cmds.setAttr("defaultRenderGlobals.startFrame", startFrame)
	cmds.setAttr("defaultRenderGlobals.endFrame", endFrame)
	cmds.setAttr("defaultRenderGlobals.outFormatControl", False)
	cmds.setAttr("defaultRenderGlobals.animation", True)
	cmds.setAttr("defaultRenderGlobals.putFrameBeforeExt", True)
	cmds.setAttr("defaultRenderGlobals.extensionPadding", 4)
	cmds.setAttr("defaultRenderGlobals.periodInExt", True)

	cmds.BatchRender()
	return

def changeFrame(frameValue):
	if (frameValue == 1):
		cmds.currentTime(1)
	else:
		cmds.currentTime(frameValue*5-5)
		
	alpha = cmds.getAttr ("leftShoulder.rotateY")
	beta = cmds.getAttr ("leftElbow.rotateY")
	gamma = cmds.getAttr ("leftWrist.rotateY")
	theta = cmds.getAttr ("leftFinger1.rotateY")
	zeta = 2 * cmds.getAttr ("leftShoulder.rotateX") + 3 * cmds.getAttr ("leftElbow.rotateX") + 2 * cmds.getAttr ("leftWrist.rotateX") + 1 * cmds.getAttr ("leftFinger1.rotateX")
		
	cmds.floatField(alphaField,e=True,value=alpha)
	cmds.floatSlider(alphaSlider,e=True,value=alpha)
	cmds.floatField(betaField,e=True,value=beta)
	cmds.floatSlider(betaSlider,e=True,value=beta)
	cmds.floatField(gammaField,e=True,value=gamma)
	cmds.floatSlider(alphaSlider,e=True,value=gamma)
	cmds.floatField(thetaField,e=True,value=theta)
	cmds.floatSlider(thetaSlider,e=True,value=theta)
	cmds.floatField(zetaField,e=True,value=zeta)
	cmds.floatSlider(zetaSlider,e=True,value=zeta)
	return

def changeAlpha(amt):
	cmds.setAttr("leftShoulder.rotateY",amt)
	cmds.setAttr("rightShoulder.rotateY",-1*amt)
	cmds.floatField(alphaField,e=True,v=amt)
	cmds.floatSlider(alphaSlider,e=True,v=amt)
	return

def changeBeta(amt):
	cmds.setAttr("leftElbow.rotateY",amt)
	cmds.setAttr("rightElbow.rotateY",-1*amt)
	cmds.floatField(betaField,e=True,v=amt)
	cmds.floatSlider(betaSlider,e=True,v=amt)
	return
	
def changeGamma(amt):
	cmds.setAttr("leftWrist.rotateY",amt)
	cmds.setAttr("rightWrist.rotateY",-1*amt)
	cmds.floatField(gammaField,e=True,v=amt)
	cmds.floatSlider(gammaSlider,e=True,v=amt)
	return
	
def changeTheta(amt):
	cmds.setAttr("leftFinger1.rotateY",amt)
	cmds.setAttr("rightFinger1.rotateY",-1*amt)
	cmds.floatField(thetaField,e=True,v=amt)
	cmds.floatSlider(thetaSlider,e=True,v=amt)
	return
	
def changeZeta(amt):
	distributor = amt / 8
	cmds.setAttr("leftShoulder.rotateX",distributor * 2)
	cmds.setAttr("rightShoulder.rotateX",distributor * 2)
	cmds.setAttr("leftElbow.rotateX",distributor * 3)
	cmds.setAttr("rightElbow.rotateX",distributor * 3)
	cmds.setAttr("leftWrist.rotateX",distributor * 2)
	cmds.setAttr("rightWrist.rotateX",distributor * 2)
	cmds.setAttr("leftFinger1.rotateX",distributor * 1)
	cmds.setAttr("rightFinger1.rotateX",distributor * 1)
	
	cmds.floatField(zetaField,e=True,v=amt)
	cmds.floatSlider(zetaSlider,e=True,v=amt)
	return
	
def saveValues():
	cmds.setKeyframe("leftShoulder.rx")
	cmds.setKeyframe("rightShoulder.rx")
	cmds.setKeyframe("leftShoulder.ry")
	cmds.setKeyframe("rightShoulder.ry")
	
	cmds.setKeyframe("leftElbow.rx")
	cmds.setKeyframe("rightElbow.rx")
	cmds.setKeyframe("leftElbow.ry")
	cmds.setKeyframe("rightElbow.ry")
	
	cmds.setKeyframe("leftWrist.rx")
	cmds.setKeyframe("rightWrist.rx")
	cmds.setKeyframe("leftWrist.ry")
	cmds.setKeyframe("rightWrist.ry")
	
	cmds.setKeyframe("leftFinger1.rx")
	cmds.setKeyframe("rightFinger1.rx")
	cmds.setKeyframe("leftFinger1.ry")
	cmds.setKeyframe("rightFinger1.ry")
	return

cmds.setAttr("mentalrayGlobals.optimizeAnimateDetection", False)
	
fileName = "batRender"
directory = cmds.workspace(q=True, rd=True) + "/images/batRenders/"
totalFileName = directory + fileName
editor = 'renderView'
extension = getFormat(cmds.getAttr('defaultRenderGlobals.imageFormat'))

startFrame = 1
endFrame = 90
nFramePadLength=4

controlWin = cmds.window(title="Bat Control Interface", wh=(512,300))

form = cmds.formLayout()

frameMenu = cmds.optionMenu(label="Key Frames", cc='changeFrame(cmds.optionMenu(frameMenu,q=True,sl=True))')
cmds.menuItem(frameMenu,label='1')
for i in range (1,19):
    cmds.menuItem(frameMenu,label=str(i+1))
    cmds.currentTime(5*i)
cmds.currentTime(1)

alpha = cmds.getAttr ("leftShoulder.rotateY")
beta = cmds.getAttr ("leftElbow.rotateY")
gamma = cmds.getAttr ("leftWrist.rotateY")
theta = cmds.getAttr ("leftFinger1.rotateY")
zeta = 2 * cmds.getAttr ("leftShoulder.rotateX") + 3 * cmds.getAttr ("leftElbow.rotateX") + 2 * cmds.getAttr ("leftWrist.rotateX") + 1 * cmds.getAttr ("leftFinger1.rotateX")

alphaField = cmds.floatField(w=80,value=alpha, cc='changeAlpha(cmds.floatField(alphaField,q=True,value=True))')
alphaSlider = cmds.floatSlider( min=-100, max=100, step=0.1, value=alpha, dc='changeAlpha(cmds.floatSlider(alphaSlider,q=True,value=True))' )
alphaText = cmds.text(label="Alpha")

betaField = cmds.floatField(w=80,value=beta, cc='changeBeta(cmds.floatField(betaField,q=True,value=True))')
betaSlider = cmds.floatSlider( min=-100, max=100, step=0.1, value=beta, dc='changeBeta(cmds.floatSlider(betaSlider,q=True,value=True))'   )
betaText = cmds.text(label="Beta")

gammaField = cmds.floatField(w=80,value=gamma, cc='changeGamma(cmds.floatField(gammaField,q=True,value=True))')
gammaSlider = cmds.floatSlider( min=-100, max=100, step=0.1, value=gamma, dc='changeGamma(cmds.floatSlider(gammaSlider,q=True,value=True))'   )
gammaText = cmds.text(label="Gamma")

thetaField = cmds.floatField(w=80,value=theta, cc='changeTheta(cmds.floatField(thetaField,q=True,value=True))')
thetaSlider = cmds.floatSlider( min=-100, max=100, step=0.1, value=theta, dc='changeTheta(cmds.floatSlider(thetaSlider,q=True,value=True))'   )
thetaText = cmds.text(label="Theta")

zetaField = cmds.floatField(w=80,value=zeta,cc='changeZeta(cmds.floatField(zetaField,q=True,value=True))')
zetaSlider = cmds.floatSlider( min=-100, max=100, step=0.1, value=zeta, dc='changeZeta(cmds.floatSlider(zetaSlider,q=True,value=True))'   )
zetaText = cmds.text(label="Zeta")

saveAngleButton = cmds.button(label='Save Angles at This Frame', c='saveValues()')

fNLabel = cmds.text(label="File Name")
fN = cmds.textField(w=200,text=fileName)

dirLabel = cmds.text(label="Directory")
dir = cmds.textField(w=200,text=directory)

nFPLabel = cmds.text(label="Frame Pad Length")
nFP = cmds.textField(w=200,text=nFramePadLength)

startFrameLabel = cmds.text(label="Start Frame")
startFrame = cmds.textField(w=200,text=startFrame)

endFrameLabel = cmds.text(label="End Frame")
endFrame = cmds.textField(w=200,text=endFrame)

directory = cmds.textField(dir,q=True,text=True)
fileName = cmds.textField(fN,query=True,text=True)
nFramePadLength = cmds.textField(nFP,query=True,text=True)

saveCurrentFrameButton = cmds.button(label='Save Image of Current Frame')
saveEveryFrameButton = cmds.button(label='Save Image of All Frames',command='safeBatchRender(1,90)')
saveSelectedFramesButton = cmds.button(label='Save Image of Selected Frames',command='safeBatchRender(int(cmds.textField(startFrame,q=True,text=True)),int(cmds.textField(endFrame,q=True,text=True)))')
cancelRenderButton = cmds.button(label='Cancel Current Render',command='cmds.batchRender()')

cmds.formLayout( form,
            edit=True,
            attachForm=[
                (frameMenu, 'top', 15),
                (frameMenu, 'left', 400),
                
                (alphaText, 'top', 15),
                (alphaText, 'left', 10),
                (alphaField, 'top', 40),
                (alphaField, 'left', 5),
				(alphaSlider, 'top', 60),
                (alphaSlider, 'left', 5),
                (alphaSlider, 'right', 430),
                
                (betaText, 'top', 15),
                (betaText, 'left', 110),
                (betaField, 'top', 40),
                (betaField, 'left', 105),
				(betaSlider, 'top', 60),
                (betaSlider, 'left', 105),
                (betaSlider, 'right', 330),
                
                (gammaText, 'top', 15),
                (gammaText, 'left', 210),
                (gammaField, 'top', 40),
                (gammaField, 'left', 205),
				(gammaSlider, 'top', 60),
                (gammaSlider, 'left', 205),
                (gammaSlider, 'right', 230),
                
                (thetaText, 'top', 15),
                (thetaText, 'left', 310),
                (thetaField, 'top', 40),
                (thetaField, 'left', 305),
				(thetaSlider, 'top', 60),
                (thetaSlider, 'left', 305),
                (thetaSlider, 'right', 130),
                
                (zetaText, 'top', 80),
                (zetaText, 'left', 10),
                (zetaField, 'top', 105),
                (zetaField, 'left', 5),
                (zetaSlider, 'top', 125),
                (zetaSlider, 'left', 5),
                (zetaSlider, 'right', 430),
           
                (saveAngleButton, 'top', 105),
                (saveAngleButton, 'left', 110),
                
                (fNLabel, 'top', 150),
                (fNLabel, 'left', 10),
                (fN, 'top', 147),
                (fN, 'left', 100),
                
                (dirLabel, 'top', 175),
                (dirLabel, 'left', 10),
                (dir, 'top', 172),
                (dir, 'left', 100),
                
                (nFPLabel, 'top', 200),
                (nFPLabel, 'left', 10),
                (nFP, 'top', 197),
                (nFP, 'left', 100),
                
                (startFrameLabel, 'top', 227),
                (startFrameLabel, 'left', 10),
                (startFrame, 'top', 225),
                (startFrame, 'left', 100),
                
                (endFrameLabel, 'top', 257),
                (endFrameLabel, 'left', 10),
                (endFrame, 'top', 252),
                (endFrame, 'left', 100),
                
                (saveCurrentFrameButton, 'top', 150),
                (saveCurrentFrameButton, 'left', 325),
                
                (saveEveryFrameButton, 'top', 175),
                (saveEveryFrameButton, 'left', 325),
                
                (saveSelectedFramesButton, 'top', 225),
                (saveSelectedFramesButton, 'left', 325),
				
				(cancelRenderButton, 'top', 250),
                (cancelRenderButton, 'left', 325)
                ]
        )
cmds.showWindow(controlWin)