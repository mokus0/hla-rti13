#ifndef ___n_RTIambServices_h__
#define ___n_RTIambServices_h__

#include "wrap/common.h"

ccall void *wrap_new_RTIambassador(void **out_exc);
ccall void wrap_delete_RTIambassador(void *amb, void **out_exc);

////////////////////////////////////
// Federation Management Services //
////////////////////////////////////

ccall void wrap_createFederationExecution(void *amb, const char *executionName, const char *fed, void **out_exc);
ccall void wrap_destroyFederationExecution(void *amb, const char *executionName, void **out_exc);
ccall RTI_ULong wrap_joinFederationExecution (void *amb, const char *yourName, const char *executionName, void *federateAmbassadorReference, void **out_exc);
ccall void wrap_resignFederationExecution(void *amb, int theAction, void **out_exc);

///// void registerFederationSynchronizationPoint (       
/////   const char *label,  // supplied C4
/////   const char *theTag) // supplied C4
///// void registerFederationSynchronizationPoint (       
/////   const char                *label,    // supplied C4
/////   const char                *theTag,   // supplied C4
/////   const FederateHandleSet&   syncSet)  // supplied C4      
///// void synchronizationPointAchieved (      
/////   const char *label) // supplied C4
///// void requestFederationSave (    
/////   const char     *label,   // supplied C4
/////   const FedTime&  theTime) // supplied C4
///// void requestFederationSave ( 
/////   const char *label)     // supplied C4
///// void federateSaveBegun ()
///// void federateSaveComplete ()
///// void federateSaveNotComplete ()
///// void requestFederationRestore (    
/////   const char *label) // supplied C4
///// void federateRestoreComplete ()
///// void federateRestoreNotComplete ()

/////////////////////////////////////
// Declaration Management Services //
/////////////////////////////////////

ccall void wrap_publishObjectClass(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc);
ccall void wrap_unpublishObjectClass(void *amb, RTI_ULong theClass, void **out_exc);
ccall void wrap_publishInteractionClass(void *amb, RTI_ULong theClass, void **out_exc);
ccall void wrap_unpublishInteractionClass(void *amb, RTI_ULong theClass, void **out_exc);
ccall void wrap_subscribeObjectClassAttributes(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc);
ccall void wrap_unsubscribeObjectClass(void *amb, RTI_ULong theClass, void **out_exc);
ccall void wrap_subscribeInteractionClass(void *amb, RTI_ULong theClass, HsBool active, void **out_exc);
ccall void wrap_unsubscribeInteractionClass(void *amb, RTI_ULong theClass, void **out_exc);

////////////////////////////////
// Object Management Services //
////////////////////////////////

ccall RTI_ULong wrap_registerObjectInstance_withName(void *amb, RTI_ULong theClass, const char *theObject, void **out_exc);
ccall RTI_ULong wrap_registerObjectInstance(void *amb, RTI_ULong theClass, void **out_exc);

///// EventRetractionHandle                               // returned C3
///// updateAttributeValues (
/////         ObjectHandle                 theObject,     // supplied C1
/////   const AttributeHandleValuePairSet& theAttributes, // supplied C4
/////   const FedTime&                     theTime,       // supplied C4
/////   const char                        *theTag)        // supplied C4

///// void updateAttributeValues (
/////         ObjectHandle                 theObject,     // supplied C1
/////   const AttributeHandleValuePairSet& theAttributes, // supplied C4
/////   const char                        *theTag)        // supplied C4

ccall RTI_ULong wrap_sendInteractionAtTime(void *amb, RTI_ULong theInteraction, void *theParameters, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_sendInteraction(void *amb, RTI_ULong theInteraction, void *theParameters, const char *theTag, void **out_exc);

///// EventRetractionHandle                 // returned C3
///// deleteObjectInstance (
/////         ObjectHandle    theObject,    // supplied C1
/////   const FedTime&        theTime,      // supplied C4
/////   const char           *theTag)       // supplied C4

///// void deleteObjectInstance (
/////         ObjectHandle    theObject,    // supplied C1
/////   const char           *theTag)       // supplied C4

///// void localDeleteObjectInstance (
/////   ObjectHandle    theObject)       // supplied C1

///// void changeAttributeTransportationType (
/////         ObjectHandle             theObject,     // supplied C1
/////   const AttributeHandleSet&      theAttributes, // supplied C4
/////         TransportationHandle     theType)       // supplied C1

///// void changeInteractionTransportationType (
/////   InteractionClassHandle theClass, // supplied C1
/////   TransportationHandle   theType)  // supplied C1

///// void requestObjectAttributeValueUpdate (
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet& theAttributes) // supplied C4

ccall void wrap_requestClassAttributeValueUpdate(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc);

///////////////////////////////////
// Ownership Management Services //
///////////////////////////////////

///// void unconditionalAttributeOwnershipDivestiture (
/////         ObjectHandle                  theObject,     // supplied C1
/////   const AttributeHandleSet&           theAttributes) // supplied C4

///// void negotiatedAttributeOwnershipDivestiture (
/////         ObjectHandle                  theObject,     // supplied C1
/////   const AttributeHandleSet&           theAttributes, // supplied C4
/////   const char                         *theTag)        // supplied C4

///// void attributeOwnershipAcquisition (
/////         ObjectHandle        theObject,         // supplied C1
/////   const AttributeHandleSet& desiredAttributes, // supplied C4
/////   const char               *theTag)            // supplied C4

///// void attributeOwnershipAcquisitionIfAvailable (
/////         ObjectHandle        theObject,         // supplied C1
/////   const AttributeHandleSet& desiredAttributes) // supplied C4

///// AttributeHandleSet*                        // returned C6
///// attributeOwnershipReleaseResponse (
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet& theAttributes) // supplied C4

///// void cancelNegotiatedAttributeOwnershipDivestiture (
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet& theAttributes) // supplied C4

///// void cancelAttributeOwnershipAcquisition (
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet& theAttributes) // supplied C4

///// void queryAttributeOwnership (
/////   ObjectHandle    theObject,    // supplied C1
/////   AttributeHandle theAttribute) // supplied C1

///// Boolean                          // returned C3
///// isAttributeOwnedByFederate (
/////   ObjectHandle    theObject,     // supplied C1
/////   AttributeHandle theAttribute)  // supplied C1

//////////////////////////////
// Time Management Services //
//////////////////////////////

ccall void wrap_enableTimeRegulation(void *amb, const void *theFederateTime, const void *theLookahead, void **out_exc);
ccall void wrap_disableTimeRegulation(void *amb, void **out_exc);
ccall void wrap_enableTimeConstrained(void *amb, void **out_exc);
ccall void wrap_disableTimeConstrained(void *amb, void **out_exc);
ccall void wrap_timeAdvanceRequest(void *amb, void *theTime, void **out_exc);

///// void timeAdvanceRequestAvailable (
///// const FedTime& theTime) // supplied C4

///// void nextEventRequest (
/////   const FedTime& theTime) // supplied C4

///// void nextEventRequestAvailable (
/////   const FedTime& theTime) // supplied C4

///// void flushQueueRequest (
/////   const FedTime& theTime) // supplied C4

///// void enableAsynchronousDelivery()

///// void disableAsynchronousDelivery()

///// void queryLBTS (
/////   FedTime& theTime) // returned C5

///// void queryFederateTime (
/////   FedTime& theTime) // returned C5

///// void queryMinNextEventTime (
/////   FedTime& theTime) // returned C5

///// void modifyLookahead (
/////   const FedTime& theLookahead) // supplied C4

///// void queryLookahead (
/////    FedTime& theTime) // returned C5

///// void retract (
/////   EventRetractionHandle theHandle) // supplied C1

///// void changeAttributeOrderType (
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet& theAttributes, // supplied C4
/////         OrderingHandle      theType)       // supplied C1

///// void changeInteractionOrderType (
/////   InteractionClassHandle theClass, // supplied C1
/////   OrderingHandle         theType)  // supplied C1

//////////////////////////////////
// Data Distribution Management //
//////////////////////////////////

ccall void *wrap_createRegion(void *amb, RTI_ULong theSpace, RTI_ULong numberOfExtents, void **out_exc);
ccall void wrap_notifyAboutRegionModification(void *amb, void *theRegion, void **out_exc);
ccall void wrap_deleteRegion(void *amb, void *theRegion, void **out_exc);
ccall RTI_ULong wrap_registerObjectInstanceWithRegion(void *amb, RTI_ULong theClass, const char *theObject, RTI_ULong theAttributes[], void *theRegions[], RTI_ULong theNumberOfHandles, void **out_exc);

///// ObjectHandle                              // returned C3
///// registerObjectInstanceWithRegion (
/////   ObjectClassHandle theClass,             // supplied C1
/////   AttributeHandle   theAttributes[],      // supplied C4
/////   Region           *theRegions[],         // supplied C4
/////   ULong             theNumberOfHandles)   // supplied C1

///// void associateRegionForUpdates (
/////         Region             &theRegion,     // supplied C4
/////         ObjectHandle        theObject,     // supplied C1
/////   const AttributeHandleSet &theAttributes) // supplied C4

///// void unassociateRegionForUpdates (
/////   Region       &theRegion,     // supplied C4
/////   ObjectHandle  theObject)     // supplied C1

///// void subscribeObjectClassAttributesWithRegion (
/////         ObjectClassHandle   theClass,      // supplied C1
/////         Region             &theRegion,     // supplied C4
/////   const AttributeHandleSet &attributeList, // supplied C4
/////         Boolean        active = RTI_TRUE)

///// void unsubscribeObjectClassWithRegion (
/////   ObjectClassHandle theClass,          // supplied C1
/////   Region           &theRegion)         // supplied C4

ccall void wrap_subscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, HsBool active, void **out_exc);
ccall void wrap_unsubscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void **out_exc);

///// EventRetractionHandle                                // returned C3
///// sendInteractionWithRegion (
/////         InteractionClassHandle       theInteraction, // supplied C1
/////   const ParameterHandleValuePairSet &theParameters,  // supplied C4
/////   const FedTime&                     theTime,        // supplied C4
/////   const char                        *theTag,         // supplied C4
/////   const Region                      &theRegion)      // supplied C4

///// void sendInteractionWithRegion (
/////         InteractionClassHandle       theInteraction, // supplied C1
/////   const ParameterHandleValuePairSet &theParameters,  // supplied C4
/////   const char                        *theTag,         // supplied C4
/////   const Region                      &theRegion)      // supplied C4

///// void requestClassAttributeValueUpdateWithRegion (
/////         ObjectClassHandle   theClass,      // supplied C1
/////   const AttributeHandleSet &theAttributes, // supplied C4
/////   const Region             &theRegion)     // supplied C4

//////////////////////////
// RTI Support Services //
//////////////////////////

ccall RTI_ULong wrap_getObjectClassHandle(void *amb, const char *theName, void **out_exc);

///// char *                         // returned C6    
///// getObjectClassName (
/////   ObjectClassHandle theHandle) // supplied C1

ccall RTI_ULong wrap_getAttributeHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);

///// char *                          // returned C6 
///// getAttributeName (
/////   AttributeHandle   theHandle,  // supplied C1
/////   ObjectClassHandle whichClass) // supplied C1

ccall RTI_ULong wrap_getInteractionClassHandle(void *amb, const char *theName, void **out_exc);

///// char *                              // returned C6 
///// getInteractionClassName (
/////   InteractionClassHandle theHandle) // supplied C1

ccall RTI_ULong wrap_getParameterHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);

///// char *                               // returned C6
///// getParameterName (
/////   ParameterHandle        theHandle,  // supplied C1
/////   InteractionClassHandle whichClass) // supplied C1

///// ObjectHandle                 // returned C3
///// getObjectInstanceHandle (
/////   const char *theName)       // supplied C4

///// char *                     // returned C6  
///// getObjectInstanceName (
/////   ObjectHandle theHandle)  // supplied C1

ccall RTI_ULong wrap_getRoutingSpaceHandle(void *amb, const char *theName, void **out_exc);

///// char *                         // returned C6
///// getRoutingSpaceName (
/////    /* const */ SpaceHandle theHandle) // supplied C4

ccall RTI_ULong wrap_getDimensionHandle(void *amb, const char *theName, RTI_ULong whichSpace, void **out_exc);

///// char *                        // returned C6
///// getDimensionName (
/////   DimensionHandle theHandle,  // supplied C1
/////   SpaceHandle     whichSpace) // supplied C1

///// SpaceHandle                      // returned C3
///// getAttributeRoutingSpaceHandle (
/////   AttributeHandle   theHandle,   // supplied C1
/////   ObjectClassHandle whichClass)  // supplied C1

///// ObjectClassHandle            // returned C3
///// getObjectClass (
/////   ObjectHandle theObject)    // supplied C1

///// SpaceHandle                             // returned C3
///// getInteractionRoutingSpaceHandle (
/////   InteractionClassHandle   theHandle)   // supplied C1

///// TransportationHandle      // returned C3
///// getTransportationHandle (
/////   const char *theName)    // supplied C4

///// char *                            // returned C6 
///// getTransportationName (
/////   TransportationHandle theHandle) // supplied C1

///// OrderingHandle         // returned C3
///// getOrderingHandle (
/////   const char *theName) // supplied C4

///// char *                      // returned C6 
///// getOrderingName (
/////   OrderingHandle theHandle) // supplied C1

///// void enableClassRelevanceAdvisorySwitch()

///// void disableClassRelevanceAdvisorySwitch()

ccall void wrap_enableAttributeRelevanceAdvisorySwitch(void *amb, void **out_exc);

///// void disableAttributeRelevanceAdvisorySwitch()

///// void enableAttributeScopeAdvisorySwitch()

///// void disableAttributeScopeAdvisorySwitch()

///// void enableInteractionRelevanceAdvisorySwitch()

///// void disableInteractionRelevanceAdvisorySwitch()

///// Boolean // returned C3
///// tick ()

ccall HsBool wrap_tick_minimum_maximum(void *amb, double min, double max, void **out_exc);

///// RTIambassador()

///// ~RTIambassador()

///// RegionToken
///// getRegionToken(
/////   Region *)

///// Region *
///// getRegion(
/////   RegionToken)

#endif /* ___n_RTIambServices_h__ */
