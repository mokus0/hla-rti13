#ifndef ___n_hsFederateAmb_h__
#define ___n_hsFederateAmb_h__

#include "wrap/common.h"

typedef void (*VoidFunc)();
typedef void (*ConstPtr_to_Void)(const void *);
typedef void (*ConstPtr_to_ULong_to_Void)(const void *, RTI_ULong);
typedef void (*ConstPtrX2_to_Void)(const void *, const void *);
typedef void (*ULong_to_Void)(RTI_ULong);
typedef void (*ULong_to_ConstPtr_to_Void)(RTI_ULong, const void *);
typedef void (*ULong_to_ConstPtrX2_to_Void)(RTI_ULong, const void *, const void *);
typedef void (*ULong_to_ULong_to_ConstPtr_to_Void)(RTI_ULong, RTI_ULong, const void *);
typedef void (*ULongX2_to_Void)(RTI_ULong, RTI_ULong);
typedef void (*ULongX3_to_Void)(RTI_ULong, RTI_ULong, RTI_ULong);
typedef void (*ULong_to_ConstPtr_to_ConstPtr_to_ULong_to_Void)(RTI_ULong, const void *, const void *, RTI_ULong);
typedef void (*ULong_to_ConstPtrX2_to_ULongX2_to_Void)(RTI_ULong, const void *, const void *, RTI_ULong, RTI_ULong);
typedef void (*ULong_to_ConstPtrX3_to_ULongX2_to_Void)(RTI_ULong, const void *, const void *, const void *, RTI_ULong, RTI_ULong);


ccall void *wrap_new_HsFederateAmbassador(FunPtrFn releaseFunPtr, void **out_exc);
ccall void *wrap_delete_HsFederateAmbassador(void *fedAmb, void **out_exc);

ccall void hsfa_set_synchronizationPointRegistrationSucceeded           (void *, ConstPtr_to_Void);
ccall void hsfa_set_synchronizationPointRegistrationFailed              (void *, ConstPtr_to_Void);
ccall void hsfa_set_announceSynchronizationPoint                        (void *, ConstPtrX2_to_Void);
ccall void hsfa_set_federationSynchronized                              (void *, ConstPtr_to_Void);
ccall void hsfa_set_initiateFederateSave                                (void *, ConstPtr_to_Void);
ccall void hsfa_set_federationSaved                                     (void *, VoidFunc);
ccall void hsfa_set_federationNotSaved                                  (void *, VoidFunc);
ccall void hsfa_set_requestFederationRestoreSucceeded                   (void *, ConstPtr_to_Void);
ccall void hsfa_set_requestFederationRestoreFailed                      (void *, ConstPtrX2_to_Void);
ccall void hsfa_set_federationRestoreBegun                              (void *, VoidFunc);
ccall void hsfa_set_initiateFederateRestore                             (void *, ConstPtr_to_ULong_to_Void);
ccall void hsfa_set_federationRestored                                  (void *, VoidFunc);
ccall void hsfa_set_federationNotRestored                               (void *, VoidFunc);
ccall void hsfa_set_startRegistrationForObjectClass                     (void *, ULong_to_Void);
ccall void hsfa_set_stopRegistrationForObjectClass                      (void *, ULong_to_Void);
ccall void hsfa_set_turnUpdatesOnForObjectInstance                      (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_turnUpdatesOffForObjectInstance                     (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_requestAttributeOwnershipAssumption                 (void *, ULong_to_ConstPtrX2_to_Void);
ccall void hsfa_set_attributeOwnershipDivestitureNotification           (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_attributeOwnershipAcquisitionNotification           (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_attributeOwnershipUnavailable                       (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_requestAttributeOwnershipRelease                    (void *, ULong_to_ConstPtrX2_to_Void);
ccall void hsfa_set_confirmAttributeOwnershipAcquisitionCancellation    (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_informAttributeOwnership                            (void *, ULongX3_to_Void);
ccall void hsfa_set_attributeIsNotOwned                                 (void *, ULongX2_to_Void);
ccall void hsfa_set_attributeOwnedByRTI                                 (void *, ULongX2_to_Void);
ccall void hsfa_set_discoverObjectInstance                              (void *, ULong_to_ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_timeRegulationEnabled                               (void *, ConstPtr_to_Void);
ccall void hsfa_set_timeConstrainedEnabled                              (void *, ConstPtr_to_Void);
ccall void hsfa_set_timeAdvanceGrant                                    (void *, ConstPtr_to_Void);
ccall void hsfa_set_requestRetraction                                   (void *, ULongX2_to_Void);
ccall void hsfa_set_turnInteractionsOn                                  (void *, ULong_to_Void);
ccall void hsfa_set_turnInteractionsOff                                 (void *, ULong_to_Void);
ccall void hsfa_set_removeObjectInstance                                (void *, ULong_to_ConstPtrX2_to_ULongX2_to_Void);
ccall void hsfa_set_attributesInScope                                   (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_attributesOutOfScope                                (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_provideAttributeValueUpdate                         (void *, ULong_to_ConstPtr_to_Void);
ccall void hsfa_set_reflectAttributeValues                              (void *, ULong_to_ConstPtrX3_to_ULongX2_to_Void);
ccall void hsfa_set_receiveInteraction                                  (void *, ULong_to_ConstPtrX3_to_ULongX2_to_Void);

#endif /* ___n_hsFederateAmb_h__ */
