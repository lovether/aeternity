# coding: utf-8

"""
    Aeternity Epoch

    This is the [Aeternity](https://www.aeternity.com/) Epoch API.  # noqa: E501

    OpenAPI spec version: 0.7.0
    Contact: apiteam@aeternity.com
    Generated by: https://github.com/swagger-api/swagger-codegen.git
"""


import pprint
import re  # noqa: F401

import six


class PubKey(object):
    """NOTE: This class is auto generated by the swagger code generator program.

    Do not edit the class manually.
    """

    """
    Attributes:
      swagger_types (dict): The key is attribute name
                            and the value is attribute type.
      attribute_map (dict): The key is attribute name
                            and the value is json key in definition.
    """
    swagger_types = {
        'pub_key': 'str'
    }

    attribute_map = {
        'pub_key': 'pub_key'
    }

    def __init__(self, pub_key=None):  # noqa: E501
        """PubKey - a model defined in Swagger"""  # noqa: E501

        self._pub_key = None
        self.discriminator = None

        if pub_key is not None:
            self.pub_key = pub_key

    @property
    def pub_key(self):
        """Gets the pub_key of this PubKey.  # noqa: E501


        :return: The pub_key of this PubKey.  # noqa: E501
        :rtype: str
        """
        return self._pub_key

    @pub_key.setter
    def pub_key(self, pub_key):
        """Sets the pub_key of this PubKey.


        :param pub_key: The pub_key of this PubKey.  # noqa: E501
        :type: str
        """

        self._pub_key = pub_key

    def to_dict(self):
        """Returns the model properties as a dict"""
        result = {}

        for attr, _ in six.iteritems(self.swagger_types):
            value = getattr(self, attr)
            if isinstance(value, list):
                result[attr] = list(map(
                    lambda x: x.to_dict() if hasattr(x, "to_dict") else x,
                    value
                ))
            elif hasattr(value, "to_dict"):
                result[attr] = value.to_dict()
            elif isinstance(value, dict):
                result[attr] = dict(map(
                    lambda item: (item[0], item[1].to_dict())
                    if hasattr(item[1], "to_dict") else item,
                    value.items()
                ))
            else:
                result[attr] = value

        return result

    def to_str(self):
        """Returns the string representation of the model"""
        return pprint.pformat(self.to_dict())

    def __repr__(self):
        """For `print` and `pprint`"""
        return self.to_str()

    def __eq__(self, other):
        """Returns true if both objects are equal"""
        if not isinstance(other, PubKey):
            return False

        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        """Returns true if both objects are not equal"""
        return not self == other
