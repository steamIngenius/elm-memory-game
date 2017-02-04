'use strict'
const electron = require('electron')

const app = electron.app
const BrowserWindow = electron.BrowserWindow

let mainWindow

app.on('ready', createWindow)

function createWindow () {
  mainWindow = new BrowserWindow({
    width: 850,
    height: 850
  })

  mainWindow.loadURL(`file://${__dirname}/src/static/index.html`)
  mainWindow.webContents.openDevTools()
  mainWindow.on('closed', () => {
    mainWindow = null
  })
}

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})
