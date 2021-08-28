import React from 'react';
import {
  Box,
  Heading,
  Text,
  Flex
} from '@chakra-ui/react';
import { useSelector } from '../../reducer';
import { FilePreview } from './FileUpload';

function Label(props: { children: React.ReactNode }) {
  return (
    <Text fontSize="md" fontFamily="mono" color="brand.darkGray">
      {props.children}
    </Text>
  );
}

export default function Confirmation() {
  const selectedFile = useSelector(s => s.createNft.selectedFile);
  const collections = useSelector(s => s.collections.collections);
  const fields = useSelector(s => s.createNft.fields);
  const collectionAddress = useSelector(s => s.createNft.collectionAddress);

  return (
    <Box>
      <Heading size="lg" mb={4}>
        Confirm Details
      </Heading>
      <Flex w="100%" justify="center" mb={8}>
        <FilePreview file={selectedFile!} />
      </Flex>
      <Label>Collection</Label>
      <Text fontSize="md" mb={[2, 4]}>
        {collections[collectionAddress!]?.metadata?.name}
      </Text>
      <Label>Name</Label>
      <Text fontSize="md" mb={[2, 4]}>
        {fields.name}
      </Text>
      <Label>Description</Label>
      <Text fontSize="md" mb={[2, 4]}>
        {fields.description || 'No description provided'}
      </Text>
    </Box>
  );
}
