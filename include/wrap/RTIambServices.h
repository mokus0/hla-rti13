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
ccall RTI_ULong wrap_sendInteractionAtTime(void *amb, RTI_ULong theInteraction, void *theParameters, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc);
ccall void wrap_sendInteraction(void *amb, RTI_ULong theInteraction, void *theParameters, const char *theTag, void **out_exc);
ccall RTI_ULong wrap_registerObjectInstance(void *amb, RTI_ULong theClass, void **out_exc);
ccall void wrap_requestClassAttributeValueUpdate(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc);

///////////////////////////////////
// Ownership Management Services //
///////////////////////////////////

//////////////////////////////
// Time Management Services //
//////////////////////////////

ccall void wrap_enableTimeRegulation(void *amb, const void *theFederateTime, const void *theLookahead, void **out_exc);
ccall void wrap_disableTimeRegulation(void *amb, void **out_exc);
ccall void wrap_enableTimeConstrained(void *amb, void **out_exc);
ccall void wrap_disableTimeConstrained(void *amb, void **out_exc);
ccall void wrap_timeAdvanceRequest(void *amb, void *theTime, void **out_exc);

//////////////////////////////////
// Data Distribution Management //
//////////////////////////////////

ccall void *wrap_createRegion(void *amb, RTI_ULong theSpace, RTI_ULong numberOfExtents, void **out_exc);
ccall void wrap_notifyAboutRegionModification(void *amb, void *theRegion, void **out_exc);
ccall void wrap_deleteRegion(void *amb, void *theRegion, void **out_exc);
ccall RTI_ULong wrap_registerObjectInstanceWithRegion(void *amb, RTI_ULong theClass, const char *theObject, RTI_ULong theAttributes[], void *theRegions[], RTI_ULong theNumberOfHandles, void **out_exc);
ccall void wrap_subscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, HsBool active, void **out_exc);
ccall void wrap_unsubscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void **out_exc);

//////////////////////////
// RTI Support Services //
//////////////////////////

ccall RTI_ULong wrap_getObjectClassHandle(void *amb, const char *theName, void **out_exc);
ccall RTI_ULong wrap_getAttributeHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);
ccall RTI_ULong wrap_getInteractionClassHandle(void *amb, const char *theName, void **out_exc);
ccall RTI_ULong wrap_getParameterHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc);
ccall RTI_ULong wrap_getRoutingSpaceHandle(void *amb, const char *theName, void **out_exc);
ccall RTI_ULong wrap_getDimensionHandle(void *amb, const char *theName, RTI_ULong whichSpace, void **out_exc);
ccall void wrap_enableAttributeRelevanceAdvisorySwitch(void *amb, void **out_exc);
ccall HsBool wrap_tick_minimum_maximum(void *amb, double min, double max, void **out_exc);

#endif /* ___n_RTIambServices_h__ */
