import React from 'react';
import {
  Divider,
  FormControl,
  FormLabel,
  Input,
  Text,
  Textarea
} from '@chakra-ui/react';

import { useSelector, useDispatch } from '../../reducer';
import {
  updateField
} from '../../reducer/slices/createNft';
import CollectionSelect from './CollectionSelect';

const DESCRIPTION_PLACEHOLDER =
  'Share some information on the habitat where this butterfly was photographed and any unique features observed.';

export default function Form() {
  const state = useSelector(s => s.createNft);
  const dispatch = useDispatch();
  const { name, description } = state.fields;
  return (
    <>
      <CollectionSelect />
      <FormControl paddingBottom={6}>
        <FormLabel fontFamily="mono">NFT Name</FormLabel>
        <Input
          autoFocus={true}
          placeholder="Input your NFT name"
          value={name || ''}
          onChange={e =>
            dispatch(updateField({ name: 'name', value: e.target.value }))
          }
        />
      </FormControl>
      <FormControl paddingBottom={6}>
        <FormLabel fontFamily="mono" display="flex">
          Description
          <Text marginLeft={2} color="brand.lightGray">
            (Optional)
          </Text>
        </FormLabel>
        <Textarea
          minHeight="100px"
          fontFamily="mono"
          placeholder={DESCRIPTION_PLACEHOLDER}
          value={description || ''}
          onChange={e =>
            dispatch(
              updateField({ name: 'description', value: e.target.value })
            )
          }
        />
      </FormControl>
      <Divider borderColor="brand.lightBlue" opacity="1" marginY={10} />
    </>
  );
}
