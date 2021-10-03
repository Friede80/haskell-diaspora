#!/bin/bash
stack run
aws --profile update-blog s3 sync ./out s3://haskelldiaspora.com