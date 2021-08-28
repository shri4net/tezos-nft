import React from 'react';
import { Heading } from '@chakra-ui/react';
import { useSelector, useDispatch } from '../../../reducer';
import {
  selectCollection
} from '../../../reducer/slices/collections';
import CollectionTab from './CollectionTab';

export default function Sidebar() {
  const state = useSelector(s => s.collections);
  const dispatch = useDispatch();
  return (
    <>
      <Heading px={4} pt={6} pb={4} size="md" color="brand.darkGray">
        My Collections
      </Heading>
      {state.collections[state.globalCollection] ? (
        <CollectionTab
          key={state.globalCollection}
          selected={state.globalCollection === state.selectedCollection}
          onSelect={address => dispatch(selectCollection(address))}
          {...state.collections[state.globalCollection]}
        />
      ) : null}
    </>
  );
}
