{\rtf1\ansi\ansicpg1252\cocoartf2867
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 //######################################################################################################## \
//#                                                                                                    #\\\\\
//#                           LANDTRENDR GREATEST DISTURBANCE MAPPING                                  #\\\\\
//#                                                                                                    #\\\\\
//########################################################################################################\
\
\
// Orginal creation date: 2018-10-07\
//    Updates ongoing through 11-2020 \
//    modified by Eric KONAN 04-2022 and last modification on 06-2025\
//                |ericlandry41@gmail.com\
// \
// authors: Justin Braaten | jstnbraaten@gmail.com\
//         Zhiqiang Yang  | zhiqiang.yang@oregonstate.edu\
//         Robert Kennedy | kennedygeospatial@gmail.com\
// \
// For more information, see \
// website: https://github.com/eMapR/LT-GEE\
\
//##########################################################################################\
// START INPUTS\
//##########################################################################################\
\
// define collection parameters\
var startYear = 2020;\
var endYear = 2024;\
var startDay = '11-01';\
var endDay = '03-31';\
var index = 'NBR';\
var maskThese = ['cloud', 'shadow', 'snow', 'water'];\
\
// define landtrendr parameters\
var runParams = \{ \
  maxSegments:            10,\
  spikeThreshold:         1.0,\
  vertexCountOvershoot:   3,\
  preventOneYearRecovery: true,\
  recoveryThreshold:      1.0,\
  pvalThreshold:          0.15,\
  bestModelProportion:    0.9,\
  minObservationsNeeded:  10\
\};\
\
// define change parameters\
var changeParams = \{\
  delta:  'loss',\
  sort:   'greatest',\
  year:   \{checked:true, start:2020, end:2024\},\
  mag:    \{checked:true, value:100,  operator:'>'\},\
  dur:    \{checked:true, value:4,    operator:'<'\},\
  preval: \{checked:true, value:300,  operator:'>'\},\
  mmu:    \{checked:true, value:11\},\
  \
\};\
\
//##########################################################################################\
// END INPUTS\
//##########################################################################################\
\
// load the LandTrendr.js module\
// Note:  This is a copy of adapted for use by World Bank trainees. \
\
var ltgee = require('users/ericlandry41/default:ERP_MR1/ldtrv020.js'); \
\
\
// Load in the asset describing the bounds of analysis\
//  To change the area of interest, change this to your own \
//  area of interest. \
\
\
\
\
\
// add index to changeParams object\
changeParams.index = index;\
\
// Run landtrendr.  View the .runLT function within the \
// LandTrendr_V2.4WB.js library for more information on how \
// these parameters are passed to the core LandTrendr algorithm on GEE\
\
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, [], runParams, maskThese);\
\
// The returned value is a GEE-image that must be manipulated to create\
// an actual map-like image. This is done through a second function. \
// to get the change map layers\
var changeImg = ltgee.getChangeMap(lt, changeParams);\
\
// set visualization dictionaries\
var palette = ['#9400D3', '#4B0082', '#0000FF', '#00FF00', '#FFFF00', '#FF7F00', '#FF0000'];\
var yodVizParms = \{\
  min: startYear,\
  max: endYear,\
  palette: palette\
\};\
\
var magVizParms = \{\
  min: 100,\
  max: 800,\
  palette: palette\
\};\
\
\
// display the change attribute map - note that there are other layers - print changeImg to console to see all\
Map.centerObject(aoi, 11);\
Map.addLayer(changeImg.select(['mag']), magVizParms, 'Magnitude of Change');\
Map.addLayer(changeImg.select(['yod']), yodVizParms, 'Year of Detection');\
\
\
// export change data to google drive\
\
var exportImg = changeImg.clip(aoi).unmask(0).short();\
Export.image.toDrive(\{\
  image: exportImg, \
  description: 'Me_ltv2_MP_2020-2024', \
  folder: 'ERP', \
  fileNamePrefix: 'Me_ltv3_MP_2020-2024', \
  region: aoi, \
  scale: 30, \
  crs: 'EPSG:4326', //NOTE:  the CRS information should be changed to match the best projection for your study area\
  maxPixels: 1e13\
\});\
\
\
// Output layers\
// Band 1:  'yod'  Year of disturbance \
// Band 2:  'mag'  Magnitude of disturbance (in units of original spectral index)\
// Band 3:  'dur'  Duration of the segment identified as disturbed. \
// Band 4:  'preval'  The spectral value of the vertex preceding the disturbance segment\
// Band 5:  'rate'  Magnitude / duration of disturbance\
// Band 6:  'dsnr'  Magnitude of disturbance / RMSE of per-pixel fit\
\
\
\
}