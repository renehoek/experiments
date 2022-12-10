CURRENT = "CURRENT"
CURRENT_TO_KR = "CURRENT_TO_KR"
RN_9999 = "9999"

from types import MappingProxyType
import copy
class GroupPolicy:

    _policy = [MappingProxyType({
        'origin_site_name': 'ledenraad', 'origin_site_loc_code': CURRENT,
        'groups_to_create': [
            {'target_site_name': 'ledenraad', 'target_site_loc_code': CURRENT, 'groups': ['lezer']},
            {'target_site_name': 'ledenraad', 'target_site_loc_code': RN_9999, 'groups': ['lezer']}
        ],
        'groups_to_create_optional': {
            'jongerenforum_9999': {
                'target_site_name': 'jongerenforum', 'target_site_loc_code': RN_9999, 'groups': ['lezer']
            },
            'adviesraad1': {
                'target_site_name': 'adviesraad1', 'target_site_loc_code': CURRENT_TO_KR, 'groups': ['lezer']
            },
            'adviesraad2': {
                'target_site_name': 'adviesraad2', 'target_site_loc_code': CURRENT_TO_KR, 'groups': ['lezer']
            },
            'adviesraad3': {
                'target_site_name': 'adviesraad3', 'target_site_loc_code': CURRENT_TO_KR, 'groups': ['lezer']
            },
            'adviesraad4': {
                'target_site_name': 'adviesraad4', 'target_site_loc_code': CURRENT_TO_KR, 'groups': ['lezer']
            },
            'adviesraad5': {
                'target_site_name': 'adviesraad5', 'target_site_loc_code': CURRENT_TO_KR, 'groups': ['lezer']
            },
        }
    })]

    def __get__(self, instance, owner):
        return self._policy

    def __set__(self, instance, value):
        raise AttributeError("Setting is not allowed")
