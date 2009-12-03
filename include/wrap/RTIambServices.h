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

ccall void wrap_registerFederationSynchronizationPoint(void *amb, const char *label, const char *theTag, void **out_exc);
ccall void wrap_registerFederationSynchronizationPoint_with_syncSet(void *amb, const char *label, const char *theTag, void *syncSet, void **out_exc);

ccall void wrap_synchronizationPointAchieved(void *amb, const char *label, void **out_exc);

ccall void wrap_requestFederationSaveAtTime(void *amb, const char *label, void *theTime, void **out_exc);
ccall void wrap_requestFederationSave(void *amb, const char *label, void **out_exc);

ccall void wrap_federateSaveBegun(void *amb, void **out_exc);
ccall void wrap_federateSaveComplete(void *amb, void **out_exc);
ccall void wrap_federateSaveNotComplete(void *amb, void **out_exc);

ccall void wrap_requestFederationRestore(void *amb, const char *label, void **out_exc);

ccall void wrap_federateRestoreComplete(void *amb, void **out_exc);
ccall void wrap_federateRestoreNotComplete(void *amb, void **out_exc);

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

ccall void wrap_updateAttributeValuesAtTime(void *amb, RTI_ULong theObject, void *theAttributes, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_updateAttributeValues(void *amb, RTI_ULong theObject, void *theAttributes, const char *theTag, void **out_exc);

ccall RTI_ULong wrap_sendInteractionAtTime(void *amb, RTI_ULong theInteraction, void *theParameters, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_sendInteraction(void *amb, RTI_ULong theInteraction, void *theParameters, const char *theTag, void **out_exc);

ccall void wrap_deleteObjectInstanceAtTime(void *amb, RTI_ULong theObject, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_deleteObjectInstance(void *amb, RTI_ULong theObject, const char *theTag, void **out_exc);

ccall void wrap_localDeleteObjectInstance(void *amb, RTI_ULong theObject, void **out_exc);

ccall void wrap_changeAttributeTransportationType(void *amb, RTI_ULong theObject, void *theAttributes, RTI_ULong theType, void **out_exc);
ccall void wrap_changeInteractionTransportationType(void *amb, RTI_ULong theClass, RTI_ULong theType, void **out_exc);

ccall void wrap_requestObjectAttributeValueUpdate(void *amb, RTI_ULong theObject, void *attributeList, void **out_exc);
ccall void wrap_requestClassAttributeValueUpdate(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc);

///////////////////////////////////
// Ownership Management Services //
///////////////////////////////////

ccall void wrap_unconditionalAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc);
ccall void wrap_negotiatedAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, char *theTag, void **out_exc);

ccall void wrap_attributeOwnershipAcquisition(void *amb, RTI_ULong theObject, void *theAttributes, char *theTag, void **out_exc);
ccall void wrap_attributeOwnershipAcquisitionIfAvailable(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc);

ccall void *wrap_attributeOwnershipReleaseResponse(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc);
ccall void wrap_cancelNegotiatedAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc);
ccall void wrap_cancelAttributeOwnershipAcquisition(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc);

ccall void wrap_queryAttributeOwnership(void *amb, RTI_ULong theObject, RTI_ULong theAttribute, void **out_exc);
ccall HsBool wrap_isAttributeOwnedByFederate(void *amb, RTI_ULong theObject, RTI_ULong theAttribute, void **out_exc);

//////////////////////////////
// Time Management Services //
//////////////////////////////

ccall void wrap_enableTimeRegulation(void *amb, const void *theFederateTime, const void *theLookahead, void **out_exc);
ccall void wrap_disableTimeRegulation(void *amb, void **out_exc);
ccall void wrap_enableTimeConstrained(void *amb, void **out_exc);
ccall void wrap_disableTimeConstrained(void *amb, void **out_exc);

ccall void wrap_timeAdvanceRequest(void *amb, void *theTime, void **out_exc);
ccall void wrap_timeAdvanceRequestAvailable(void *amb, void *theTime, void **out_exc);
ccall void wrap_nextEventRequest(void *amb, void *theTime, void **out_exc);
ccall void wrap_nextEventRequestAvailable(void *amb, void *theTime, void **out_exc);
ccall void wrap_flushQueueRequest(void *amb, void *theTime, void **out_exc);

ccall void wrap_enableAsynchronousDelivery(void *amb, void **out_exc);
ccall void wrap_disableAsynchronousDelivery(void *amb, void **out_exc);

ccall void wrap_queryLBTS(void *amb, void *theTime, void **out_exc);
ccall void wrap_queryFederateTime(void *amb, void *theTime, void **out_exc);
ccall void wrap_queryMinNextEventTime(void *amb, void *theTime, void **out_exc);

ccall void wrap_modifyLookahead(void *amb, void *theTime, void **out_exc);
ccall void wrap_queryLookahead(void *amb, void *theTime, void **out_exc);

ccall void wrap_retract(void *amb, RTI_ULong theSerial, RTI_ULong theFederate, void **out_exc);

ccall void wrap_changeAttributeOrderType(void *amb, RTI_ULong theObject, void *theAttributes, RTI_ULong theType, void **out_exc);
ccall void wrap_changeInteractionOrderType(void *amb, RTI_ULong theClass, RTI_ULong theType, void **out_exc);

//////////////////////////////////
// Data Distribution Management //
//////////////////////////////////

ccall void *wrap_createRegion(void *amb, RTI_ULong theSpace, RTI_ULong numberOfExtents, void **out_exc);
ccall void wrap_notifyAboutRegionModification(void *amb, void *theRegion, void **out_exc);
ccall void wrap_deleteRegion(void *amb, void *theRegion, void **out_exc);

ccall RTI_ULong wrap_registerObjectInstanceWithRegion_withName(void *amb, RTI_ULong theClass, const char *theObject, RTI_ULong theAttributes[], void *theRegions[], RTI_ULong theNumberOfHandles, void **out_exc);
ccall RTI_ULong wrap_registerObjectInstanceWithRegion(void *amb, RTI_ULong theClass, RTI_ULong theAttributes[], void *theRegions[], RTI_ULong theNumberOfHandles, void **out_exc);

ccall void wrap_associateRegionForUpdates(void *amb, void *theRegion, RTI_ULong theObject, void *theAttributes, void **out_exc);
ccall void wrap_unassociateRegionForUpdates(void *amb, void *theRegion, RTI_ULong theObject, void **out_exc);

ccall void wrap_subscribeObjectClassAttributesWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void *theAttributes, HsBool active, void **out_exc);
ccall void wrap_unsubscribeObjectClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void **out_exc);

ccall void wrap_subscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, HsBool active, void **out_exc);
ccall void wrap_unsubscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void **out_exc);

ccall RTI_ULong wrap_sendInteractionWithRegionAtTime(void *amb, RTI_ULong theInteraction, void *theParameters, void *theTime, const char *theTag, void *theRegion, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_sendInteractionWithRegion(void *amb, RTI_ULong theInteraction, void *theParameters, const char *theTag, void *theRegion, void **out_exc);

ccall void wrap_requestClassAttributeValueUpdateWithRegion(void *amb, RTI_ULong theClass, void *theAttributes, void *theRegion, void **out_exc);

//////////////////////////
// RTI Support Services //
//////////////////////////

ccall RTI_ULong wrap_getObjectClassHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getObjectClassName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getAttributeHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);
ccall const char *wrap_getAttributeName(void *amb, RTI_ULong theHandle, RTI_ULong whichClass, void **out_exc);

ccall RTI_ULong wrap_getInteractionClassHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getInteractionClassName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getParameterHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);
ccall const char *wrap_getParameterName(void *amb, RTI_ULong theHandle, RTI_ULong whichClass, void **out_exc);

ccall RTI_ULong wrap_getObjectInstanceHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getObjectInstanceName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getRoutingSpaceHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getRoutingSpaceName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getDimensionHandle(void *amb, const char *theName, RTI_ULong whichSpace, void **out_exc);
ccall const char *wrap_getDimensionName(void *amb, RTI_ULong theHandle, RTI_ULong whichSpace, void **out_exc);

ccall RTI_ULong wrap_getAttributeRoutingSpaceHandle(void *amb, RTI_ULong theAttribute, RTI_ULong whichClass, void **out_exc);

ccall RTI_ULong wrap_getObjectClass(void *amb, RTI_ULong theObject, void **out_exc);

ccall RTI_ULong wrap_getInteractionRoutingSpaceHandle(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getTransportationHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getTransportationName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall RTI_ULong wrap_getOrderingHandle(void *amb, const char *theName, void **out_exc);
ccall const char *wrap_getOrderingName(void *amb, RTI_ULong theHandle, void **out_exc);

ccall void wrap_enableClassRelevanceAdvisorySwitch(void *amb, void **out_exc);
ccall void wrap_disableClassRelevanceAdvisorySwitch(void *amb, void **out_exc);

ccall void wrap_enableAttributeRelevanceAdvisorySwitch(void *amb, void **out_exc);
ccall void wrap_disableAttributeRelevanceAdvisorySwitch(void *amb, void **out_exc);

ccall void wrap_enableAttributeScopeAdvisorySwitch(void *amb, void **out_exc);
ccall void wrap_disableAttributeScopeAdvisorySwitch(void *amb, void **out_exc);

ccall void wrap_enableInteractionRelevanceAdvisorySwitch(void *amb, void **out_exc);
ccall void wrap_disableInteractionRelevanceAdvisorySwitch(void *amb, void **out_exc);

ccall HsBool wrap_tick(void *amb, void **out_exc);
ccall HsBool wrap_tick_minimum_maximum(void *amb, double min, double max, void **out_exc);

ccall RTI_ULong wrap_getRegionToken(void *amb, void *theRegion, void **out_exc);
ccall void     *wrap_getRegion(void *amb, RTI_ULong theRegion, void **out_exc);

#endif /* ___n_RTIambServices_h__ */
